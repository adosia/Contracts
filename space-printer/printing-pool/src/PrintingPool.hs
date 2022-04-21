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
data CustomDatumType =  PrintingPool        PrintingPoolType        |
                        OfferInformation    OfferInformationType    |
                        PrintingInformation PrintingInfoType        |
                        CurrentlyShipping   ShippingInfoType
PlutusTx.makeIsDataIndexed ''CustomDatumType  [ ('PrintingPool,        0)
                                              , ('OfferInformation,    1)
                                              , ('PrintingInformation, 2)
                                              , ('CurrentlyShipping,   3)
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
                          Ship
PlutusTx.makeIsDataIndexed ''CustomRedeemerType [ ('Remove,  0)
                                                , ('Update,  1)
                                                , ('Offer,   2)
                                                , ('Accept,  3)
                                                , ('Deny,    4)
                                                , ('Cancel,  5)
                                                , ('Ship,    6)
                                                ]
PlutusTx.makeLift ''CustomRedeemerType
-------------------------------------------------------------------------------
-- | mkValidator :: Data -> Datum -> Redeemer -> ScriptContext -> Bool
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
              { let contValue = validatingValue - Ada.lovelaceValueOf (ppOfferPrice ppt) + Ada.lovelaceValueOf (ppOfferPrice ppt')
              ; let a = traceIfFalse "Incorrect Signer"       $ txSignedBy info (ppCustomerPKH ppt)                                   -- customer must sign it
              ; let b = traceIfFalse "Incorrect Token Return" $ checkContTxOutForValue continuingTxOutputs contValue                  -- token must go back to script
              ; let c = traceIfFalse "Too Many Script Inputs" $ checkForNScriptInputs txInputs (1 :: Integer)                         -- single script input
              ; let d = traceIfFalse "Incorrect New State"    $ ppt == ppt'                                                           -- printer must change the datum 
              ; let e = traceIfFalse "Not Enough ADA On UTXO" $ Value.valueOf contValue Ada.adaSymbol Ada.adaToken > ppOfferPrice ppt'
              ; all (==(True :: Bool)) [a,b,c,d,e]
              }
            _ -> False
        Offer ->  
          case embeddedDatum continuingTxOutputs of
            (OfferInformation oit) -> do
              { let a = traceIfFalse "Incorrect Signer"       $ txSignedBy info (oiPrinterPKH oit)                         -- The printer must sign it
              ; let b = traceIfFalse "Offer: Incrt Tkn Cont"  $ checkContTxOutForValue continuingTxOutputs validatingValue -- token must go back to script
              ; let c = traceIfFalse "Incorrect New State"    $ ppt === oit                                                -- printer must change the datum 
              ; let d = traceIfFalse "Not Enough ADA On UTXO" $ Value.valueOf validatingValue Ada.adaSymbol Ada.adaToken > ppOfferPrice ppt
              ; all (==(True :: Bool)) [a,b,c,d]
              }
            _ -> False
        _ -> False
    (OfferInformation oit) ->
      case redeemer of
        Accept -> 
          case embeddedDatum continuingTxOutputs of
            (PrintingInformation pit) -> do
              {
              ; let a = traceIfFalse "Incorrect Signer"     $ txSignedBy info (oiCustomerPKH oit)                        -- customer must sign it
              ; let b = traceIfFalse "Incorrect Token Cont" $ checkContTxOutForValue continuingTxOutputs validatingValue -- token must go back to script
              ; let c = traceIfFalse "Incorrect Printer"    $ oit === pit                                                -- Only the stage can change
              ; let d = traceIfFalse "SingleScript Inputs"  $ checkForNScriptInputs txInputs (1 :: Integer)                 -- single script input
              ; all (==(True :: Bool)) [a,b,c,d]
              }
            _ -> False
        Deny ->
          case embeddedDatum continuingTxOutputs of
            (PrintingPool ppt) -> do
              { let a = traceIfFalse "Incorrect State"      $ ppt === oit                                              -- the token is getting an offer
              ; let b = traceIfFalse "Incorrect Signer"     $ txSignedBy info (oiPrinterPKH oit) || txSignedBy info (oiCustomerPKH oit)
              ; let c = traceIfFalse "Incorrect Token Cont" $ checkContTxOutForValue continuingTxOutputs validatingValue -- token must go back to script
              ; let d = traceIfFalse "SingleScript Inputs"  $ checkForNScriptInputs txInputs (1 :: Integer)              -- single script input
              ; all (==(True :: Bool)) [a,b,c,d]
              }
            _ -> False
        Update ->
          case embeddedDatum continuingTxOutputs of
            (OfferInformation oit') -> do
              { let a = traceIfFalse "Incorrect State"      $ oit == oit'                                                -- change the print time
              ; let b = traceIfFalse "Incorrect Signer"     $ txSignedBy info (oiPrinterPKH oit)
              ; let c = traceIfFalse "Incorrect Token Cont" $ checkContTxOutForValue continuingTxOutputs validatingValue -- token must go back to script
              ; let d = traceIfFalse "SingleScript Inputs"  $ checkForNScriptInputs txInputs (1 :: Integer)              -- single script input
              ; all (==(True :: Bool)) [a,b,c,d]
              }
            _ -> False
        _ -> False
    (PrintingInformation pit) ->
      case redeemer of
        Cancel ->
          case embeddedDatum continuingTxOutputs of
            (PrintingPool ppt) -> do
              { let a = traceIfFalse "Incorrect State"      $ pit === ppt                                                -- the token is getting an offer
              ; let b = traceIfFalse "Incorrect Signer"     $ txSignedBy info (piPrinterPKH pit)                         -- customer must sign it
              ; let c = traceIfFalse "Incorrect Token Cont" $ checkContTxOutForValue continuingTxOutputs validatingValue -- token must go back to script
              ; let d = traceIfFalse "SingleScript Inputs"  $ checkForNScriptInputs txInputs (1 :: Integer)              -- single script input
              ; all (==(True :: Bool)) [a,b,c,d]
              }
            _ -> False
        Remove -> 
          case embeddedDatum continuingTxOutputs of
            (PrintingPool ppt) -> do
              { let a = traceIfFalse "Incorrect State"       $ pit === ppt                                                -- the token is getting an offer
              ; let b = traceIfFalse "Incorrect Signer"      $ txSignedBy info (piCustomerPKH pit)                        -- customer must sign it
              ; let c = traceIfFalse "Incorrect Token Cont"  $ checkContTxOutForValue continuingTxOutputs validatingValue -- token must go back to script
              ; let d = traceIfFalse "SingleScript Inputs"   $ checkForNScriptInputs txInputs (1 :: Integer)              -- single script input
              ; let e = traceIfFalse "Job Is Still Printing" $ not $ overlaps (lockInterval (piPrintTime pit)) validityRange
              ; all (==(True :: Bool)) [a,b,c,d,e]
              }
            _ -> False
        Ship ->
          case embeddedDatum continuingTxOutputs of
            (CurrentlyShipping sit) -> do
              { let a = traceIfFalse "Incorrect Signer"     $ txSignedBy info (siPrinterPKH sit)                         -- printer must sign
              ; let b = traceIfFalse "Incorrect Token Cont" $ checkContTxOutForValue continuingTxOutputs validatingValue -- token must go back to script
              ; let c = traceIfFalse "Incorrect New State"  $ pit === sit                                                -- job is done printing and is now shipping
              ; let d = traceIfFalse "SingleScript Inputs"  $ checkForNScriptInputs txInputs (1 :: Integer)              -- single script input
              ; all (==(True :: Bool)) [a,b,c,d]
              }
            _ -> False
        _ -> False
    (CurrentlyShipping sit) ->
      case redeemer of
        Accept -> do
          { let a = traceIfFalse "Incorrect Signer"       $ txSignedBy info (siCustomerPKH sit)                                                                     -- customer must sign it
          ; let b = traceIfFalse "Incorrect Token payout" $ checkTxOutForValueAtPKH txOutputs (siCustomerPKH sit) (validatingValue - priceValue (siOfferPrice sit)) -- customer gets token back
          ; let c = traceIfFalse "Incorrect Price payout" $ checkTxOutForValueAtPKH txOutputs (siPrinterPKH sit) (priceValue $ siOfferPrice sit)                    -- printer gets paid
          ; let d = traceIfFalse "SingleScript Inputs"    $ checkForNScriptInputs txInputs (1 :: Integer)                                                           -- single script input
          ; all (==(True :: Bool)) [a,b,c,d]
          }
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
    validatingValue = 
      case findOwnInput context of
        Nothing     -> traceError "No Input to Validate."  -- This should never be hit.
        Just input  -> txOutValue $ txInInfoResolved input

    priceValue :: Integer -> Value
    priceValue price = Ada.lovelaceValueOf price

    embeddedDatum :: [TxOut] -> CustomDatumType
    embeddedDatum [] = datum
    embeddedDatum (txOut:txOuts) =
      case txOutDatumHash txOut of
        Nothing -> embeddedDatum txOuts
        Just dh ->
          case findDatum dh info of
            Nothing        -> embeddedDatum txOuts
            Just (Datum datum') -> Data.Maybe.fromMaybe datum (PlutusTx.fromBuiltinData datum')

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
-------------------------------------------------------------------------------
-- | Off Chain
-------------------------------------------------------------------------------
type Schema = Endpoint ""  ()

contract :: AsContractError e => Contract () Schema e ()
contract = selectList [] >> contract