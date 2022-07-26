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
module CheckFuncs
  ( checkForNScriptInputs
  , checkContTxOutForValue
  , checkTxOutForValueAtPKH
  , checkTxOutForValueAtAddr
  , lockInterval
  , createAddress
  ) where
import           Ledger                    hiding ( singleton )
import           PlutusTx.Prelude
import qualified Plutus.V1.Ledger.Value    as Value
import qualified Plutus.V1.Ledger.Time     as Time
import qualified Plutus.V1.Ledger.Interval as Interval
import           Plutus.V1.Ledger.Credential

{- |
  Author   : The Ancient Kraken
  Copyright: 2022
  Version  : Rev 2
-}

-------------------------------------------------------------------------
-- | Lock for neg inf to endingTime
-------------------------------------------------------------------------
lockInterval :: Integer -> Interval POSIXTime
lockInterval endingTime= Interval.to (integerToPOSIX endingTime)
  where
    integerToPOSIX :: Integer -> POSIXTime
    integerToPOSIX x = Time.fromMilliSeconds $ Time.DiffMilliSeconds x

-------------------------------------------------------------------------
-- | Create an address, either payment or staking depending on inputs
-------------------------------------------------------------------------
createAddress :: PubKeyHash -> PubKeyHash -> Address
createAddress pkh sc = if getPubKeyHash sc == emptyByteString then Address (PubKeyCredential pkh) Nothing else Address (PubKeyCredential pkh) (Just $ StakingHash $ PubKeyCredential sc)

-------------------------------------------------------------------------
-- | Search each TxOut for the value.
-------------------------------------------------------------------------
checkContTxOutForValue :: [TxOut] -> Value -> Bool
checkContTxOutForValue [] _val = False
checkContTxOutForValue (x:xs) val
  | checkVal  = True
  | otherwise = checkContTxOutForValue xs val
  where
    checkVal :: Bool
    checkVal = Value.geq (txOutValue x) val

-------------------------------------------------------------------------
-- Search each TxOut for the correct address and value.
-------------------------------------------------------------------------
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

-------------------------------------------------------------------------------
-- | Search each TxOut for an pkh and value.
-------------------------------------------------------------------------------
checkTxOutForValueAtAddr :: [TxOut] -> Address -> Value -> Bool
checkTxOutForValueAtAddr [] _addr _val = False
checkTxOutForValueAtAddr (x:xs) addr val
  | checkAddr && checkVal = True
  | otherwise             = checkTxOutForValueAtAddr xs addr val
  where
    checkAddr :: Bool
    checkAddr = txOutAddress x == addr

    checkVal :: Bool
    checkVal = Value.geq (txOutValue x) val

-------------------------------------------------------------------------
-- Force a N script utxo inputs.
-------------------------------------------------------------------------
checkForNScriptInputs :: [TxInInfo] -> Integer -> Bool
checkForNScriptInputs txInputs = loopInputs txInputs 0
  where
    loopInputs :: [TxInInfo] -> Integer -> Integer -> Bool
    loopInputs []      counter nMatch = counter == nMatch
    loopInputs (x:xs) !counter nMatch = case txOutDatumHash $ txInInfoResolved x of
        Nothing -> do counter <= nMatch && loopInputs xs counter nMatch
        Just _  -> do counter <= nMatch && loopInputs xs (counter + 1) nMatch
