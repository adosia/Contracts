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
  ( checkContTxOutForValue
  , checkTxOutForValueAtPKH
  , checkForNScriptInputs
  , checkVoteWeight
  ) where

import           Ledger
import           PlutusTx.Prelude


import HelperFuncs
import DataTypes
-------------------------------------------------------------------------
-- | Check things here
-------------------------------------------------------------------------
-- | Search each TxOut for the value.
checkContTxOutForValue :: [TxOut] -> Value -> Bool
checkContTxOutForValue [] _val = False
checkContTxOutForValue (x:xs) val
  | checkVal  = True
  | otherwise = checkContTxOutForValue xs val
  where
    checkVal :: Bool
    checkVal = txOutValue x == val

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
    checkVal = txOutValue x == val

-- Force a N script utxo inputs.
checkForNScriptInputs :: [TxInInfo] -> Integer -> Bool
checkForNScriptInputs txInputs nMatch' = traceIfFalse "Too many Script Inputs." $ loopInputs txInputs 0 nMatch'
  where
    loopInputs :: [TxInInfo] -> Integer -> Integer -> Bool
    loopInputs []      counter nMatch = counter == nMatch
    loopInputs (x:xs) !counter nMatch = case txOutDatumHash $ txInInfoResolved x of
        Nothing -> do counter <= nMatch && loopInputs xs counter nMatch
        Just _  -> do counter <= nMatch && loopInputs xs (counter + 1) nMatch

checkVoteWeight :: TxInfo -> CustomDatumType -> Integer -> Bool
checkVoteWeight info datum majorityParam = txWeight >= majorityParam
  where
    txWeight = calculateWeight (txInfoSignatories info) (cdtVestingGroupPKH datum) (cdtVestingWeights datum) 0