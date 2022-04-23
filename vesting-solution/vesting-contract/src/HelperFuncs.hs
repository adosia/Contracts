{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE DerivingVia           #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE ImportQualifiedPost   #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
-- Options
{-# OPTIONS_GHC -fno-strictness               #-}
{-# OPTIONS_GHC -fno-ignore-interface-pragmas #-}
{-# OPTIONS_GHC -fno-omit-interface-pragmas   #-}
{-# OPTIONS_GHC -fobject-code                 #-}
{-# OPTIONS_GHC -fno-specialise               #-}
{-# OPTIONS_GHC -fexpose-all-unfoldings       #-}
module HelperFuncs
  ( lockInterval
  , rewardFunction
  , calculateWeight
  , embeddedDatum
  ) where
import           Ledger
import           PlutusTx.Prelude
import qualified Data.Maybe
import qualified PlutusTx
import qualified Plutus.V1.Ledger.Interval as Interval
import qualified Plutus.V1.Ledger.Time     as Time
import           DataTypes
-------------------------------------------------------------------------------
-- | Find the incoming embedded datum or return the passed in datum
-------------------------------------------------------------------------------
embeddedDatum :: CustomDatumType -> TxInfo -> [TxOut] -> CustomDatumType
embeddedDatum datum _ [] = datum
embeddedDatum datum info (txOut:txOuts) = 
  case txOutDatumHash txOut of
    Nothing                -> embeddedDatum datum info txOuts
    Just possibleDatumHash -> 
      case findDatum possibleDatumHash info of
        Nothing         -> datum
        Just (Datum d)  -> Data.Maybe.fromMaybe datum (PlutusTx.fromBuiltinData d)
-------------------------------------------------------------------------------
-- | Pick the locking interval, assume negative inf to endingTime.
-------------------------------------------------------------------------------
lockInterval :: CustomDatumType -> Interval POSIXTime
lockInterval datum = Interval.to (integerToPOSIX endingTime)
  where
    -- unix time at epoch 312
    timeTilRefEpoch :: Integer
    -- timeTilRefEpoch = 1640987100000  -- mainnet
    timeTilRefEpoch = 1640895900000  -- testnet

    -- time unit
    lengthOfDay :: Integer
    lengthOfDay = 86400000

    -- pick some day to start
    startDay :: Integer
    startDay = head $ tail $ cdtDeadlineParams datum

    -- pick some number of days for the vesting period
    lockedPeriod :: Integer
    lockedPeriod = head $ cdtDeadlineParams datum

    -- starting Time is just the reference plus how many days in nanoseconds.
    startingTime :: Integer
    startingTime = timeTilRefEpoch + startDay * lengthOfDay

    -- ending time is just starting time plus the vesting period.
    endingTime :: Integer
    endingTime = startingTime + lockedPeriod * lengthOfDay

    -- Number of milliseconds from unix time start
    integerToPOSIX :: Integer -> POSIXTime
    integerToPOSIX x = Time.fromMilliSeconds $ Time.DiffMilliSeconds x
-------------------------------------------------------------------------------
-- | Assume Linear reward
-------------------------------------------------------------------------------
rewardFunction :: CustomDatumType -> Integer
rewardFunction datum = v0 - t * deltaV
  where
    -- starting amount
    v0 :: Integer
    v0 = head $ tail $ cdtRewardParams datum

    -- amount reduced every period
    deltaV :: Integer
    deltaV = head $ cdtRewardParams datum

    -- time increment
    t :: Integer
    t = cdtVestingStage datum
-------------------------------------------------------------------------------
-- | Calculate the total voting weight from all signers of a transaction
-------------------------------------------------------------------------------
calculateWeight :: [PubKeyHash] -> [PubKeyHash] -> [Integer] -> Integer -> Integer
calculateWeight [] _ _ counter = counter
calculateWeight (signer:signers) votingGroup votingWeights counter
  | checkSigneeInGroup signer votingGroup = calculateWeight signers votingGroup votingWeights (counter + signerWeight)
  | otherwise                             = calculateWeight signers votingGroup votingWeights counter
    where
      checkSigneeInGroup :: PubKeyHash -> [PubKeyHash] -> Bool
      checkSigneeInGroup _ [] = False
      checkSigneeInGroup pkh (vestor:vestors)
        | pkh == vestor = True
        | otherwise     = checkSigneeInGroup pkh vestors

      getSignerWeight :: PubKeyHash -> [PubKeyHash] -> [Integer] -> Integer
      getSignerWeight _ [] [] = 0
      getSignerWeight _ _ []  = 0
      getSignerWeight _ [] _  = 0
      getSignerWeight pkh (vestor:vestors) (weight:weights)
        | pkh == vestor = weight
        | otherwise     = getSignerWeight pkh vestors weights
      
      signerWeight :: Integer
      signerWeight = getSignerWeight signer votingGroup votingWeights