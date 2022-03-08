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

module VestingContract
  ( vestingContractScript
  , vestingContractScriptShortBs
  , Schema
  , contract
  , CustomDatumType
  , testData'
  , testData''
  , lockInterval
  , rewardFunction
  , listLength
  ) where

import           Cardano.Api.Shelley       (PlutusScript (..), PlutusScriptV1)

import           Codec.Serialise           ( serialise )

import qualified Data.ByteString.Lazy      as LBS
import qualified Data.ByteString.Short     as SBS
import qualified Data.Maybe

import           Playground.Contract
import           Plutus.Contract

import           Ledger
import qualified Ledger.Typed.Scripts      as Scripts

import qualified PlutusTx
import           PlutusTx.Prelude

import qualified Plutus.V1.Ledger.Scripts  as Plutus
import qualified Plutus.V1.Ledger.Interval as Interval
import qualified Plutus.V1.Ledger.Time     as Time
import qualified Plutus.V1.Ledger.Value    as Value
import qualified Plutus.V1.Ledger.Ada      as Ada


{- |
  Author   : The Ancient Kraken
  Copyright: 2021
  Version  : Rev 0

  This is a vesting contract.
-}

-------------------------------------------------------------------------------
-- | Create the token sale parameters data object.
-------------------------------------------------------------------------------
data VestingContractParams = VestingContractParams
  { vcMajorityParam  :: !Integer
  -- ^ The number of keys that determines the majority.
  , vcPolicyID       :: !CurrencySymbol
  -- ^ The policy id of the vesting token.
  , vcTokenName      :: !TokenName
  -- ^ The token name of the vesting token.
  , vcProviderPKH    :: !PubKeyHash
  -- ^ The vesting as a service provider pkh
  , vcProviderProfit :: !Integer
  }
PlutusTx.makeLift ''VestingContractParams


-------------------------------------------------------------------------------
-- | Create the datum parameters data object.
-------------------------------------------------------------------------------

data CustomDatumType = CustomDatumType
  { cdtVestingStage    :: !Integer
  -- ^ The stage determines the deadline and reward.
  , cdtVestingUserPKH  :: !PubKeyHash
  -- ^ The public key hash of the receiver.
  , cdtVestingGroupPKH :: ![PubKeyHash]
  -- ^ A list public key hashes of everyone who is vesting with the contract.
  , cdtTreasuryPKH     :: !PubKeyHash
  -- ^ The public key hash of the treasury wallet.
  , cdtDeadlineParams  :: ![Integer]
  -- ^ The deadline function parameters [deltaT, t0]
  , cdtRewardParams    :: ![Integer]
  -- ^ The reward function parameters [deltaV, v0]
  }
    deriving stock (Show, Generic)
    deriving anyclass (FromJSON, ToJSON, ToSchema)
PlutusTx.unstableMakeIsData ''CustomDatumType
PlutusTx.makeLift ''CustomDatumType

-- old is a
-- new is b
instance Eq CustomDatumType where
  {-# INLINABLE (==) #-}
  a == b = ( cdtVestingStage a + 1 == cdtVestingStage    b) &&
           ( cdtVestingUserPKH   a == cdtVestingUserPKH  b) &&
           ( cdtVestingGroupPKH  a == cdtVestingGroupPKH b) &&
           ( cdtTreasuryPKH      a == cdtTreasuryPKH     b) &&
           ( cdtRewardParams     a == cdtRewardParams    b) &&
           ( head (cdtDeadlineParams a) == head (cdtDeadlineParams b)) &&
           ( head (tail (cdtDeadlineParams a)) + head (cdtDeadlineParams a) == head (tail (cdtDeadlineParams b)))

testData' :: CustomDatumType
testData' = CustomDatumType { cdtVestingStage = 0
                           , cdtVestingUserPKH = ""
                           , cdtVestingGroupPKH = [""]
                           , cdtTreasuryPKH = ""
                           , cdtDeadlineParams = [5,0]
                           , cdtRewardParams = [0,10]
                           }

testData'' :: CustomDatumType
testData'' = CustomDatumType  { cdtVestingStage = 1
                              , cdtVestingUserPKH = ""
                              , cdtVestingGroupPKH = [""]
                              , cdtTreasuryPKH = ""
                              , cdtDeadlineParams = [5,5]
                              , cdtRewardParams = [0,10]
                              }
-------------------------------------------------------------------------------
-- | Create the redeemer parameters data object.
-------------------------------------------------------------------------------

newtype CustomRedeemerType = CustomRedeemerType
  { crtAction :: Integer }
    deriving stock (Show, Generic)
    deriving anyclass (FromJSON, ToJSON, ToSchema)
PlutusTx.unstableMakeIsData ''CustomRedeemerType
PlutusTx.makeLift ''CustomRedeemerType


-------------------------------------------------------------------------------
-- | Define The Token Sale Parameters Here
-------------------------------------------------------------------------------

validator :: Plutus.Validator
validator = Scripts.validatorScript (typedValidator vc)
  where
    vc = VestingContractParams
      { vcMajorityParam = 3
      , vcPolicyID      = "57fca08abbaddee36da742a839f7d83a7e1d2419f1507fcbf3916522"
      , vcTokenName     = "CHOC"
      , vcProviderPKH   = "06c35b3567b2d8f4c3a838c44050fa785c702d532467c8bfdb85046b"
      , vcProviderProfit = 1000000
      }

-------------------------------------------------------------------------------
-- | Deadline and Reward Functions
-------------------------------------------------------------------------------

-- Pick the locking interval
lockInterval :: CustomDatumType -> Interval POSIXTime
lockInterval datum' = Interval.interval (integerToPOSIX startingTime) (integerToPOSIX endingTime)
  where
    -- unix time at epoch 312
    timeTilRefEpoch :: Integer
    -- timeTilRefEpoch = 1640987100000  -- mainnet
    timeTilRefEpoch = 1640895900000  -- testnet

    lengthOfDay :: Integer
    lengthOfDay = 1000*60*60*24

    -- pick some day to start
    startDay :: Integer
    startDay = head $ tail $ cdtDeadlineParams datum'

    -- pick some number of days for the vesting period
    lockedPeriod :: Integer
    lockedPeriod = head $ cdtDeadlineParams datum'

    startingTime :: Integer
    startingTime = timeTilRefEpoch + startDay*lengthOfDay

    endingTime :: Integer
    endingTime = startingTime + lockedPeriod*lengthOfDay

    -- Number of milliseconds from unix time start
    integerToPOSIX :: Integer -> POSIXTime
    integerToPOSIX x = Time.fromMilliSeconds $ Time.DiffMilliSeconds x

-- Assume Linear reward
rewardFunction :: CustomDatumType -> Integer
rewardFunction datum' = v0 - t * deltaV
  where
    -- starting amount
    v0 :: Integer
    v0 = head $ tail $ cdtRewardParams datum'

    -- amount reduced every period
    deltaV :: Integer
    deltaV = head $ cdtRewardParams datum'

    t :: Integer
    t = cdtVestingStage datum'

-- calculate the length of list
listLength :: [a] -> Integer
listLength arr = countHowManyElements arr 0
  where
    countHowManyElements [] counter = counter
    countHowManyElements (_:xs) counter = countHowManyElements xs (counter + 1)

-------------------------------------------------------------------------------
-- | mkValidator :: Data -> Datum -> Redeemer -> ScriptContext -> Bool
-------------------------------------------------------------------------------
{-# INLINABLE mkValidator #-}
mkValidator :: VestingContractParams -> CustomDatumType -> CustomRedeemerType -> ScriptContext -> Bool
mkValidator vc datum redeemer context
  | checkRedeemer = True
  | otherwise     = traceIfFalse "Validation Has Failed" False
    where
      -------------------------------------------------------------------------
      -- | Use the redeemer to switch validators.
      -------------------------------------------------------------------------
      checkRedeemer :: Bool
      checkRedeemer
        | action == 0 = retrieveFunds
        | action == 1 = closeVestment
        | action == 2 = petitionVote
        | otherwise   = traceIfFalse "Error: checkRedeemer Failure" True -- Set True For BYPASS

      action :: Integer
      action = crtAction redeemer

      -------------------------------------------------------------------------
      -- | On-chain endpoints
      -------------------------------------------------------------------------

      -- retrieve funds from the contract
      retrieveFunds :: Bool
      retrieveFunds = do
        { let a = traceIfFalse "Single Script Only"           checkForSingleScriptInput
        ; let b = traceIfFalse "Incorrect Signer"             $ txSignedBy (scriptContextTxInfo context) (cdtVestingUserPKH datum)
        ; let c = traceIfFalse "The Value Is Still Locked"    $ not $ overlaps (lockInterval datum) (txInfoValidRange $ scriptContextTxInfo context)
        ; let d = traceIfFalse "Incorrect Incoming Datum"     $ datum == embeddedDatum (getContinuingOutputs context)
        ; let e = traceIfFalse "Value Not Return To Script"   $ checkContTxOutForValue (getContinuingOutputs context) (validatedValue - retrieveValue)
        ; let f = traceIfFalse "Funds Not Being Retrieved"    $ checkTxOutForValueAtPKH (txInfoOutputs $ scriptContextTxInfo context) (cdtVestingUserPKH datum) retrieveValue
        ; let g = traceIfFalse "No Funds Left"                $ Value.valueOf validatedValue (vcPolicyID vc) (vcTokenName vc) > (0 :: Integer)
        ; let h = traceIfFalse "Provider Not Being Paid"      $ checkTxOutForValueAtPKH (txInfoOutputs $ scriptContextTxInfo context) (vcProviderPKH vc) (Ada.lovelaceValueOf $ vcProviderProfit vc)
        ;         traceIfFalse "Error: retrieveFunds Failure" $ all (==(True :: Bool)) [a,b,c,d,e,f,g,h]
        }

      -- close an empty vesting utxo
      closeVestment :: Bool
      closeVestment = do
        { let a = traceIfFalse "Single Script Only"           checkForSingleScriptInput
        ; let b = traceIfFalse "Funds Not Being Retrieved"    $ checkTxOutForValueAtPKH (txInfoOutputs $ scriptContextTxInfo context) (cdtTreasuryPKH datum) validatedValue
        ; let c = traceIfFalse "Funds Are Left"               $ Value.valueOf validatedValue (vcPolicyID vc) (vcTokenName vc) == (0 :: Integer)
        ;         traceIfFalse "Error: closeVestment Failure" $ all (==(True :: Bool)) [a,b,c]
        }

      -- multi sig vote off chain heavy
      petitionVote :: Bool
      petitionVote = do
        { let a = traceIfFalse "Single Script Only"          checkForSingleScriptInput
        ; let b = traceIfFalse "Not Enough Votes"            $ checkMajoritySigners (cdtVestingGroupPKH datum) 0
        ; let c = traceIfFalse "Provider Not Being Paid"     $ checkTxOutForValueAtPKH (txInfoOutputs $ scriptContextTxInfo context) (vcProviderPKH vc) (Ada.lovelaceValueOf $ vcProviderProfit vc)
        ;         traceIfFalse "Error: petitionVote Failure" $ all (==(True :: Bool)) [a,b,c]
        }
      -------------------------------------------------------------------------
      -- | Helpers
      -------------------------------------------------------------------------

      validatedValue :: Value
      validatedValue = case findOwnInput context of
          Nothing    -> traceError "No Input to Validate"
          Just input -> txOutValue $ txInInfoResolved input

      retrieveValue :: Value
      retrieveValue = Value.singleton (vcPolicyID vc) (vcTokenName vc) (rewardFunction datum)

      -- Check for embedded datum in the txout
      embeddedDatum :: [TxOut] -> CustomDatumType
      embeddedDatum [] = datum
      embeddedDatum (x:xs) = case txOutDatumHash x of
        Nothing -> embeddedDatum xs
        Just dh -> case findDatum dh $ scriptContextTxInfo context of
          Nothing         -> datum
          Just (Datum d)  -> Data.Maybe.fromMaybe datum (PlutusTx.fromBuiltinData d)

      checkMajoritySigners :: [PubKeyHash] -> Integer -> Bool
      checkMajoritySigners [] counter = let numberOfVestors = listLength (cdtVestingGroupPKH datum) in if numberOfVestors <= vcMajorityParam vc then counter == numberOfVestors else counter >= vcMajorityParam vc
      checkMajoritySigners (pkh:pkhs) !counter
        | txSignedBy (scriptContextTxInfo context) pkh = checkMajoritySigners pkhs (counter + 1)
        | otherwise = checkMajoritySigners pkhs counter

      -- | Search each TxOut for the value.
      checkContTxOutForValue :: [TxOut] -> Value -> Bool
      checkContTxOutForValue [] _val = False
      checkContTxOutForValue (x:xs) val
        | checkVal  = True
        | otherwise = checkContTxOutForValue xs val
        where
          checkVal :: Bool
          checkVal = Value.geq (txOutValue x) val

      -- Search each TxOut for the correct address and value.
      checkTxOutForValueAtPKH :: [TxOut] -> PubKeyHash -> Value -> Bool
      checkTxOutForValueAtPKH [] _pkh _val = False
      checkTxOutForValueAtPKH (x:xs) pkh val
        | checkAddr && checkVal = True
        | otherwise             = checkTxOutForValueAtPKH xs pkh val
        where
          checkAddr :: Bool
          checkAddr = txOutAddress x == pubKeyHashAddress pkh

          checkVal :: Bool
          checkVal = Value.geq (txOutValue x) val

      -- Force a single script utxo input.
      checkForSingleScriptInput :: Bool
      checkForSingleScriptInput = loopInputs (txInfoInputs $ scriptContextTxInfo context) 0
        where
          loopInputs :: [TxInInfo] -> Integer -> Bool
          loopInputs []     counter = counter == 1
          loopInputs (x:xs) counter = case txOutDatumHash $ txInInfoResolved x of
              Nothing -> do
                if counter > 1
                  then loopInputs [] counter
                  else loopInputs xs counter
              Just _  -> do
                if counter > 1
                  then loopInputs [] counter
                  else loopInputs xs (counter + 1)
-------------------------------------------------------------------------
-- | End of Validator.
-------------------------------------------------------------------------

-------------------------------------------------------------------------------
-- | This determines the data type for Datum and Redeemer.
-------------------------------------------------------------------------------

data Typed
instance Scripts.ValidatorTypes Typed where
  type instance DatumType    Typed = CustomDatumType
  type instance RedeemerType Typed = CustomRedeemerType


-------------------------------------------------------------------------------
-- | Now we need to compile the Typed Validator.
-------------------------------------------------------------------------------

typedValidator :: VestingContractParams -> Scripts.TypedValidator Typed
typedValidator vc = Scripts.mkTypedValidator @Typed
  ($$(PlutusTx.compile [|| mkValidator ||]) `PlutusTx.applyCode` PlutusTx.liftCode vc)
  $$(PlutusTx.compile  [|| wrap        ||])
    where
      wrap = Scripts.wrapValidator @CustomDatumType @CustomRedeemerType  -- @Datum @Redeemer


-------------------------------------------------------------------------------
-- | The code below is required for the plutus script compile.
-------------------------------------------------------------------------------

script :: Plutus.Script
script = Plutus.unValidatorScript validator

vestingContractScriptShortBs :: SBS.ShortByteString
vestingContractScriptShortBs = SBS.toShort . LBS.toStrict $ serialise script

vestingContractScript :: PlutusScript PlutusScriptV1
vestingContractScript = PlutusScriptSerialised vestingContractScriptShortBs


-------------------------------------------------------------------------------
-- | Off Chain
-------------------------------------------------------------------------------
type Schema =
  Endpoint "" ()

contract :: AsContractError e => Contract () Schema e ()
contract = selectList [] >> contract