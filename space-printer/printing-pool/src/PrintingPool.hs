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
{-# LANGUAGE NumericUnderscores    #-}
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
module PrintingPool
  ( printingPoolScript
  , printingPoolScriptShortBs
  , Schema
  , contract
  , CustomDatumType
  ) where
import           Cardano.Api.Shelley       (PlutusScript (..), PlutusScriptV1)
import           Codec.Serialise           ( serialise )
import qualified Data.ByteString.Lazy      as LBS
import qualified Data.ByteString.Short     as SBS
import qualified Data.Maybe
import           Ledger                    hiding ( singleton )
import qualified Ledger.Typed.Scripts      as Scripts
import           Playground.Contract
import qualified PlutusTx
import           PlutusTx.Prelude
import           Plutus.Contract
import qualified Plutus.V1.Ledger.Ada      as Ada
import qualified Plutus.V1.Ledger.Value    as Value
import qualified Plutus.V1.Ledger.Scripts  as Plutus
import           DataTypes
import           CheckFuncs
{- |
  Author   : The Ancient Kraken
  Copyright: 2021
  Version  : Rev 0

  cardano-cli 1.33.0 - linux-x86_64 - ghc-8.10
  git rev 814df2c146f5d56f8c35a681fe75e85b905aed5d

  The printing pool smart contract.
-}
-------------------------------------------------------------------------------
-- | Create the contract parameters data object.
-------------------------------------------------------------------------------
data ContractParams = ContractParams {}
PlutusTx.makeLift ''ContractParams
-------------------------------------------------------------------------------
-- | Create the datum parameters data object.
-------------------------------------------------------------------------------
data CustomDatumType =  PrintingPool      PrintingPoolType |
                        MakeOffer         PrintingInfoType |
                        CurrentlyPrinting PrintingInfoType |
                        Shipping          ShippingInfoType |
                        Registration      PrinterRegistrationType |
                        QualityAssurance  ShippingInfoType
PlutusTx.makeIsDataIndexed ''CustomDatumType  [ ('PrintingPool,      0)
                                              , ('MakeOffer,         1)
                                              , ('CurrentlyPrinting, 2)
                                              , ('Shipping,          3)
                                              , ('Registration,      4)
                                              , ('QualityAssurance,  5)
                                              ]
PlutusTx.makeLift ''CustomDatumType

-------------------------------------------------------------------------------
-- | Create the redeemer parameters data object.
-------------------------------------------------------------------------------
data CustomRedeemerType = Remove |
                          Update |
                          Offer  |
                          Accept |
                          Deny   |
                          Cancel |
                          Return |
                          Ship   |
                          Prove  |
                          Contest
PlutusTx.makeIsDataIndexed ''CustomRedeemerType [ ('Remove,  0)
                                                , ('Update,  1)
                                                , ('Offer,   2)
                                                , ('Accept,  3)
                                                , ('Deny,    4)
                                                , ('Cancel,  5)
                                                , ('Return,  6)
                                                , ('Ship,    7)
                                                , ('Prove,   8)
                                                , ('Contest, 9)
                                                ]
PlutusTx.makeLift ''CustomRedeemerType

-------------------------------------------------------------------------------
-- | mkValidator :: Data -> Datum -> Redeemer -> ScriptContext -> Bool
-- | Create a custom validator inside mkValidator.
-------------------------------------------------------------------------------
{-# INLINABLE mkValidator #-}
mkValidator :: ContractParams -> CustomDatumType -> CustomRedeemerType -> ScriptContext -> Bool
mkValidator _ datum redeemer context =
  case datum of
    (PrintingPool ppt) ->
      case redeemer of
        Remove -> do
          { let a = traceIfFalse "Incorrect Signer"       $ txSignedBy info (ppCustomerPKH ppt)                                   -- customer must sign it
          ; let b = traceIfFalse "Incorrect Token Return" $ checkTxOutForValueAtPKH txOutputs (ppCustomerPKH ppt) validatingValue -- token must go back to customer
          ; let c = traceIfFalse "Too Many Script Inputs" $ checkForNScriptInputs txInputs (1 :: Integer)                         -- single script input
          ; all (==(True :: Bool)) [a,b,c]
          }
        Update ->
          case embeddedDatum continuingTxOutputs of
            (PrintingPool ppt') -> do
              { let a = traceIfFalse "Incorrect Signer"       $ txSignedBy info (ppCustomerPKH ppt)                                   -- customer must sign it
              ; let b = traceIfFalse "Incorrect Token Return" $ checkTxOutForValueAtPKH txOutputs (ppCustomerPKH ppt) validatingValue -- token must go back to customer
              ; let c = traceIfFalse "Too Many Script Inputs" $ checkForNScriptInputs txInputs (1 :: Integer)                         -- single script input
              ; let d = traceIfFalse "Incorrect New State"    $ ppt == ppt'                                                           -- printer must change the datum 
              ; let e = traceIfFalse "Not Enough ADA On UTXO" $ Value.valueOf validatingValue Ada.adaSymbol Ada.adaToken >= ppOfferPrice ppt
              ; all (==(True :: Bool)) [a,b,c,d,e]
              }
            _ -> False
        Offer ->
          case embeddedDatum continuingTxOutputs of
            (MakeOffer pit) -> do
              { let a = traceIfFalse "Incorrect Signer"       $ txSignedBy info (piPrinterPKH pit)                         -- The printer must sign it
              ; let b = traceIfFalse "Incorrect Token Cont"   $ checkContTxOutForValue continuingTxOutputs validatingValue -- token must go back to script
              ; let c = traceIfFalse "Incorrect New State"    $ ppt === pit                                                -- printer must change the datum 
              ; let d = traceIfFalse "Too Many Script Inputs" $ checkForNScriptInputs txInputs (2 :: Integer)              -- single script input
              ; let e = traceIfFalse "Too Many Script Inputs" $ isRegisteredPrinter info txInputs (piPrinterPKH pit)       -- single script input
              ; let f = traceIfFalse "Not Enough ADA On UTXO" $ Value.valueOf validatingValue Ada.adaSymbol Ada.adaToken >= ppOfferPrice ppt
              ; all (==(True :: Bool)) [a,b,c,d,e,f]
              }
            _ -> False
        _ -> False
    (MakeOffer pit) ->
      case redeemer of
        Accept ->
          case embeddedDatum continuingTxOutputs of
            (CurrentlyPrinting pit') -> do
              {
              ; let a = traceIfFalse "Incorrect Signer"       $ txSignedBy info (piCustomerPKH pit)                        -- customer must sign it
              ; let b = traceIfFalse "Incorrect Token Cont"   $ checkContTxOutForValue continuingTxOutputs validatingValue -- token must go back to script
              ; let c = traceIfFalse "Incorrect Printer"      $ pit == pit'                                                -- Only the stage can change
              ; let d = traceIfFalse "SingleScript Inputs" $ checkForNScriptInputs txInputs (1 :: Integer)                 -- single script input
              ; all (==(True :: Bool)) [a,b,c,d]
              }
            _ -> False
        Deny   ->
          case embeddedDatum continuingTxOutputs of
            (PrintingPool ppt) -> do
              { let a = traceIfFalse "Incorrect State"      $ ppt === pit                                                -- the token is getting an offer
              ; let b = traceIfFalse "Incorrect Signer"     $ txSignedBy info (piPrinterPKH pit) || txSignedBy info (piCustomerPKH pit)
              ; let c = traceIfFalse "Incorrect Token Cont" $ checkContTxOutForValue continuingTxOutputs validatingValue -- token must go back to script
              ; let d = traceIfFalse "SingleScript Inputs"  $ checkForNScriptInputs txInputs (1 :: Integer)              -- single script input
              ; all (==(True :: Bool)) [a,b,c,d]
              }
            _ -> False
        _ -> False
    (CurrentlyPrinting pit) ->
      case redeemer of
        Cancel ->
          case embeddedDatum continuingTxOutputs of
            (PrintingPool ppt) -> do
              { let a = traceIfFalse "Incorrect State"      $ ppt === pit                                                -- the token is getting an offer
              ; let b = traceIfFalse "Incorrect Signer"     $ txSignedBy info (piPrinterPKH pit)                         -- customer must sign it
              ; let c = traceIfFalse "Incorrect Token Cont" $ checkContTxOutForValue continuingTxOutputs validatingValue -- token must go back to script
              ; let d = traceIfFalse "SingleScript Inputs"  $ checkForNScriptInputs txInputs (1 :: Integer)              -- single script input
              ; all (==(True :: Bool)) [a,b,c,d]
              }
            _ -> False
        Return ->
          case embeddedDatum continuingTxOutputs of
            (PrintingPool ppt) -> do
              { let a = traceIfFalse "Incorrect State"       $ ppt === pit                                                -- the token is getting an offer
              ; let b = traceIfFalse "Incorrect Signer"      $ txSignedBy info (piCustomerPKH pit)                         -- customer must sign it
              ; let c = traceIfFalse "Incorrect Token Cont"  $ checkContTxOutForValue continuingTxOutputs validatingValue -- token must go back to script
              ; let d = traceIfFalse "SingleScript Inputs"   $ checkForNScriptInputs txInputs (1 :: Integer)              -- single script input
              ; let e = traceIfFalse "Job Is Still Printing" $ not $ overlaps (lockInterval (piPrintTime pit)) validityRange
              ; all (==(True :: Bool)) [a,b,c,d,e]
              }
            _ -> False
        Ship ->
          case embeddedDatum continuingTxOutputs of
            (Shipping sit) -> do
              {
              ; let a = traceIfFalse "Incorrect Signer"     $ txSignedBy info (siPrinterPKH sit)                         -- printer must sign
              ; let b = traceIfFalse "Incorrect Token Cont" $ checkContTxOutForValue continuingTxOutputs validatingValue -- token must go back to script
              ; let c = traceIfFalse "Incorrect New State"  $ pit === sit                                                -- job is done printing and is now shipping
              ; let d = traceIfFalse "SingleScript Inputs"  $ checkForNScriptInputs txInputs (1 :: Integer)              -- single script input
              ; all (==(True :: Bool)) [a,b,c,d]
              }
            _ -> False
        _ -> False -- any other redeemers fail
    (Shipping sit) ->
      case redeemer of
        Accept -> do
          { let a = traceIfFalse "Incorrect Signer"       $ txSignedBy info (siCustomerPKH sit)                                                                     -- customer must sign it
          ; let b = traceIfFalse "Incorrect Token payout" $ checkTxOutForValueAtPKH txOutputs (siCustomerPKH sit) (validatingValue - priceValue (siOfferPrice sit)) -- customer gets token back
          ; let c = traceIfFalse "Incorrect Price payout" $ checkTxOutForValueAtPKH txOutputs (siPrinterPKH sit) (priceValue $ siOfferPrice sit)                    -- printer gets paid
          ; let d = traceIfFalse "SingleScript Inputs"    $ checkForNScriptInputs txInputs (1 :: Integer)                                                           -- single script input
          ; all (==(True :: Bool)) [a,b,c,d]
          }
        Contest ->
          case embeddedDatum continuingTxOutputs of
            (QualityAssurance sit') -> do
              { let a = traceIfFalse "Incorrect Signer"     $ txSignedBy info (siCustomerPKH sit)                        -- customer must sign it
              ; let b = traceIfFalse "Incorrect Token Cont" $ checkContTxOutForValue continuingTxOutputs validatingValue -- customer gets token back
              ; let c = traceIfFalse "SingleScript Inputs"  $ checkForNScriptInputs txInputs (1 :: Integer)              -- single script input
              ; let d = traceIfFalse "Incorrect New State"  $ sit == sit'                                                -- job is done printing and is now shipping
              ; all (==(True :: Bool)) [a,b,c,d]
              }
            _ -> False
        _ -> False
    (QualityAssurance sit) ->
      case redeemer of
        Accept -> do
          { let a = traceIfFalse "Incorrect Customer"  $ txSignedBy info (siCustomerPKH sit)           -- customer must sign it
          ; let b = traceIfFalse "Incorrect Printer"   $ txSignedBy info (siPrinterPKH sit)            -- customer must sign it
          ; let c = traceIfFalse "SingleScript Inputs" $ checkForNScriptInputs txInputs (1 :: Integer) -- single script input
          ; all (==(True :: Bool)) [a,b,c]
          }
        _ -> False
    (Registration prt) ->
      case redeemer of
        Remove -> do
          { let a = traceIfFalse "Incorrect Signer"       $ txSignedBy info (prPrinterPKH prt)
          ; let b = traceIfFalse "Incorrect Token payout" $ checkTxOutForValueAtPKH txOutputs (prPrinterPKH prt) validatingValue
          ; let c = traceIfFalse "SingleScript Inputs"    $ checkForNScriptInputs txInputs (1 :: Integer)
          ; all (==(True :: Bool)) [a,b,c]
          }
        Prove ->
          case embeddedDatum continuingTxOutputs of
            (Registration prt') -> do
              { let a = traceIfFalse "Incorrect Signer"       $ txSignedBy info (prPrinterPKH prt)
              ; let b = traceIfFalse "Incorrect Token Cont"   $ checkContTxOutForValue continuingTxOutputs validatingValue
              ; let c = traceIfFalse "Two Script Inputs Only" $ checkForNScriptInputs txInputs (2 :: Integer)
              ; let d = traceIfFalse "Incorrect New State"    $ prt == prt'
              ; let e = traceIfFalse "Not Enough ADA On UTXO" $ Value.valueOf validatingValue Ada.adaSymbol Ada.adaToken >= (10_000_000 :: Integer)
              ; all (==(True :: Bool)) [a,b,c,d,e]
              }
            _ -> False
        _ -> False
  where
    info :: TxInfo
    info = scriptContextTxInfo context

    continuingTxOutputs  :: [TxOut]
    continuingTxOutputs  = getContinuingOutputs context

    txInputs :: [TxInInfo]
    txInputs = txInfoInputs info

    txOutputs :: [TxOut]
    txOutputs = txInfoOutputs info

    validatingValue :: Value
    validatingValue = case findOwnInput context of
      Nothing     -> traceError "No Input to Validate."  -- This should never be hit.
      Just input  -> txOutValue $ txInInfoResolved input

    priceValue :: Integer -> Value
    priceValue price = Ada.lovelaceValueOf price

    embeddedDatum :: [TxOut] -> CustomDatumType
    embeddedDatum [] = datum
    embeddedDatum (x:xs) = case txOutDatumHash x of
      Nothing -> embeddedDatum xs
      Just dh -> case findDatum dh info of
        Nothing        -> datum
        Just (Datum d) -> Data.Maybe.fromMaybe datum (PlutusTx.fromBuiltinData d)

    validityRange :: POSIXTimeRange
    validityRange = txInfoValidRange info
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
typedValidator :: ContractParams -> Scripts.TypedValidator Typed
typedValidator cp = Scripts.mkTypedValidator @Typed
  ($$(PlutusTx.compile [|| mkValidator ||]) `PlutusTx.applyCode` PlutusTx.liftCode cp)
   $$(PlutusTx.compile [|| wrap        ||])
    where
      wrap = Scripts.wrapValidator @CustomDatumType @CustomRedeemerType  -- @Datum @Redeemer
-------------------------------------------------------------------------------
-- | Define The Token Sale Parameters Here
-------------------------------------------------------------------------------
validator :: Plutus.Validator
validator = Scripts.validatorScript (typedValidator $ ContractParams {})
-------------------------------------------------------------------------------
-- | The code below is required for the plutus script compile.
-------------------------------------------------------------------------------
script :: Plutus.Script
script = Plutus.unValidatorScript validator

printingPoolScriptShortBs :: SBS.ShortByteString
printingPoolScriptShortBs = SBS.toShort . LBS.toStrict $ serialise script

printingPoolScript :: PlutusScript PlutusScriptV1
printingPoolScript = PlutusScriptSerialised printingPoolScriptShortBs


type Schema = Endpoint ""  ()
contract :: AsContractError e => Contract () Schema e ()
contract = selectList [] >> contract