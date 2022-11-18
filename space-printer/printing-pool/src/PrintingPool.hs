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
module PrintingPool
  ( printingPoolContractScript
  , printingPoolContractScriptShortBs
  ) where
import qualified PlutusTx
import           PlutusTx.Prelude
import           Cardano.Api.Shelley            ( PlutusScript (..), PlutusScriptV2 )
import           Codec.Serialise                ( serialise )
import qualified Data.ByteString.Lazy           as LBS
import qualified Data.ByteString.Short          as SBS
import qualified Plutus.V1.Ledger.Scripts       as Scripts
import qualified Plutus.V1.Ledger.Value         as Value
import qualified Plutus.V2.Ledger.Contexts      as ContextsV2
import qualified Plutus.V2.Ledger.Api           as PlutusV2
import           Plutus.Script.Utils.V2.Scripts as Utils
import qualified Plutus.V1.Ledger.Interval      as Interval
import           DataTypes
import           UsefulFuncs
{- |
  Author   : The Ancient Kraken
  Copyright: 2022
  Version  : Rev 1
-}
-- minting policy id from the invoice minting contract.
purchaseOrderPid :: PlutusV2.CurrencySymbol
purchaseOrderPid = PlutusV2.CurrencySymbol { PlutusV2.unCurrencySymbol = createBuiltinByteString [21, 192, 170, 236, 196, 192, 157, 36, 214, 200, 64, 24, 91, 110, 162, 173, 60, 7, 196, 132, 89, 176, 52, 42, 171, 215, 149, 24] }

-------------------------------------------------------------------------------
-- | Create the datum parameters data object.
-------------------------------------------------------------------------------
data CustomDatumType =  PrintingPool      PrintingPoolType     |
                        OfferInformation  OfferInformationType |
                        CurrentlyShipping ShippingInfoType
PlutusTx.makeIsDataIndexed ''CustomDatumType  [ ('PrintingPool,      0)
                                              , ('OfferInformation,  1)
                                              , ('CurrentlyShipping, 2)
                                              ]
-------------------------------------------------------------------------------
-- | Create the redeemer parameters data object.
-------------------------------------------------------------------------------
data CustomRedeemerType = Remove               |
                          Update               |
                          Offer  MakeOfferType |
                          Accept               |
                          Cancel               |
                          Ship
PlutusTx.makeIsDataIndexed ''CustomRedeemerType [ ('Remove,  0)
                                                , ('Update,  1)
                                                , ('Offer,   2)
                                                , ('Accept,  3)
                                                , ('Cancel,  4)
                                                , ('Ship,    5)
                                                ]
-------------------------------------------------------------------------------
-- | mkValidator :: Datum -> Redeemer -> ScriptContext -> Bool
-------------------------------------------------------------------------------
{-# INLINABLE mkValidator #-}
mkValidator :: CustomDatumType -> CustomRedeemerType -> PlutusV2.ScriptContext -> Bool
mkValidator datum redeemer context =
  case datum of
    {- | PrintingPool PrintingPoolType

      Handles holding purchase orders in a pool so printers can make offers.

      An offer is a multisig agreement between the printer and customer.
    -}
    (PrintingPool ppt) ->
      case redeemer of
        -- | customer removes their PO
        Remove -> do
          { let customerPkh  = ppCustomerPKH ppt
          ; let customerSc   = ppCustomerSC  ppt
          ; let customerAddr = createAddress customerPkh customerSc
          ; let a = traceIfFalse "Incorrect Signer"    $ ContextsV2.txSignedBy info customerPkh                          -- customer must sign it
          ; let b = traceIfFalse "Value Not Returned"  $ isAddrGettingPaidExactly txOutputs customerAddr validatingValue -- token must go back to customer
          ; let c = traceIfFalse "Single Script UTxO"  $ isNInputs txInputs 1 && isNOutputs contTxOutputs 0              -- single script input
          ;         traceIfFalse "PrintingPool:Remove" $ all (==(True :: Bool)) [a,b,c]
          }
        -- | customer updates their po
        Update ->
          case getContinuingDatum contTxOutputs validatingValue of
            (PrintingPool ppt') -> do
              { let a = traceIfFalse "Incorrect Signer"     $ ContextsV2.txSignedBy info (ppCustomerPKH ppt)                                  -- customer must sign it
              ; let b = traceIfFalse "Single Script UTxO"   $ isNInputs txInputs 1 && isNOutputs contTxOutputs 1                              -- single script input
              ; let c = traceIfFalse "Wrong Purchase Order" $ Value.valueOf validatingValue purchaseOrderPid (ppPOName ppt) == (1 :: Integer) -- holding po nft
              ; let d = traceIfFalse "Incorrect New State"  $ checkPrintingPoolUpdate ppt ppt'                                                -- change pp info
              ;         traceIfFalse "PrintingPool:Update"  $ all (==(True :: Bool)) [a,b,c,d]
              }
            _ -> False
        -- | multisig agreement between customer and printer
        (Offer mot) ->
          let additionalValue = validatingValue + (adaValue $ makeOfferPrice mot)
          in case getContinuingDatum contTxOutputs additionalValue of
            (OfferInformation oit) -> do
              { let a = traceIfFalse "Bad Customer Signer"  $ ContextsV2.txSignedBy info (ppCustomerPKH ppt)                                  -- The customer must sign it
              ; let b = traceIfFalse "Bad Printer Signer"   $ ContextsV2.txSignedBy info (oiPrinterPKH oit)                                   -- The printer must sign it
              ; let c = traceIfFalse "Wrong Purchase Order" $ Value.valueOf validatingValue purchaseOrderPid (ppPOName ppt) == (1 :: Integer) -- must hold po
              ; let d = traceIfFalse "Incorrect New State"  $ checkPrintingOffer ppt oit                                                      -- printer must change the datum 
              ; let e = traceIfFalse "Single Script UTxO"   $ isNInputs txInputs 1 && isNOutputs contTxOutputs 1                              -- single script input
              ;         traceIfFalse "PrintingPool:Offer"   $ all (==(True :: Bool)) [a,b,c,d,e]
              }
            _ -> False
        -- | other endpoints should fail
        _ -> False
    {- | OfferInformation OfferInformationType

      Handles post printing pool logic. The offer has been made and agreed too.
      In this state, the printer is currently printing the design.
    -}
    (OfferInformation oit) ->
      case redeemer of
        -- | A printer is allowed to cancel an ongoing print job. 
        Cancel ->
          let offerValue      = adaValue $ oiOfferPrice oit
              additionalValue = validatingValue - offerValue
          in case getContinuingDatum contTxOutputs additionalValue of
            (PrintingPool ppt) -> do
              { let customerPkh  = ppCustomerPKH ppt
              ; let customerSc   = ppCustomerSC  ppt
              ; let customerAddr = createAddress customerPkh customerSc
              ; let a = traceIfFalse "Incorrect Signer"        $ ContextsV2.txSignedBy info (oiPrinterPKH oit)              -- customer must sign it
              ; let b = traceIfFalse "Offer Not Returned"      $ isAddrGettingPaidExactly txOutputs customerAddr offerValue -- token must go back to customer
              ; let c = traceIfFalse "Single Script UTxO"      $ isNInputs txInputs 1 && isNOutputs contTxOutputs 1         -- single script input
              ; let d = traceIfFalse "Incorrect State"         $ checkPrintingOffer ppt oit                                 -- the token is getting an offer
              ; let e = traceIfFalse "Wrong Purchase Order"    $ Value.valueOf validatingValue purchaseOrderPid (oiPOName oit) == (1 :: Integer)
              ;         traceIfFalse "OfferInformation:Cancel" $ all (==(True :: Bool)) [a,b,c,d,e]
              }
            _ -> False
        -- | A customer can remove their design from an offer after the print time has elasped.
        Remove ->
          let offerValue      = adaValue $ oiOfferPrice oit
              additionalValue = validatingValue - offerValue
          in case getContinuingDatum contTxOutputs additionalValue of
            (PrintingPool ppt) -> do
              { let customerPkh  = ppCustomerPKH ppt
              ; let customerSc   = ppCustomerSC  ppt
              ; let customerAddr = createAddress customerPkh customerSc
              ; let a = traceIfFalse "Incorrect Signer"        $ ContextsV2.txSignedBy info customerPkh                     -- customer must sign it
              ; let b = traceIfFalse "Offer Not Returned"      $ isAddrGettingPaidExactly txOutputs customerAddr offerValue -- token must go back to customer
              ; let c = traceIfFalse "Single Script UTxO"      $ isNInputs txInputs 1 && isNOutputs contTxOutputs 1         -- single script input
              ; let d = traceIfFalse "Incorrect State"         $ checkPrintingOffer ppt oit                                 -- the token is getting an offer
              ; let e = traceIfFalse "Wrong Purchase Order"    $ Value.valueOf validatingValue purchaseOrderPid (oiPOName oit) == (1 :: Integer)
              ; let f = traceIfFalse "Job Is Still Printing"   $ not $ Interval.overlaps (lockUntilTimeInterval (oiPrintTime oit)) validityRange
              ;         traceIfFalse "OfferInformation:Remove" $ all (==(True :: Bool)) [a,b,c,d,e,f]
              }
            _ -> False
        Ship ->
          case getContinuingDatum contTxOutputs validatingValue of
            (CurrentlyShipping sit) -> do
              { let a = traceIfFalse "Incorrect Signer"      $ ContextsV2.txSignedBy info (oiPrinterPKH oit)        -- printer must sign
              ; let b = traceIfFalse "Single Script UTxO"    $ isNInputs txInputs 1 && isNOutputs contTxOutputs 1   -- single script input
              ; let c = traceIfFalse "Incorrect New State"   $ checkShippingStatus oit sit                          -- job is done printing and is now shipping
              ; let d = traceIfFalse "Wrong Purchase Order"  $ Value.valueOf validatingValue purchaseOrderPid (oiPOName oit) == (1 :: Integer)
              ;         traceIfFalse "OfferInformation:Ship" $ all (==(True :: Bool)) [a,b,c,d]
              }
            _ -> False
        -- | other endpoints fail for offerinformation
        _ -> False
    {- | CurrentShipping ShippingInfoType

      Handles the shipping and payout of the final product.
    -}
    (CurrentlyShipping sit) ->
      case redeemer of
        -- mvp allows just for accepting the payments.
        Accept -> do
          { let customerPkh  = siCustomerPKH sit
          ; let customerSc   = siCustomerSC  sit
          ; let customerAddr = createAddress customerPkh customerSc
          ; let printerPkh   = siPrinterPKH sit
          ; let printerSc    = siPrinterSC  sit
          ; let printerAddr  = createAddress printerPkh printerSc
          ; let offerValue   = adaValue $ siOfferPrice sit
          ; let returnValue  = validatingValue - offerValue 
          ; let a = traceIfFalse "Incorrect Signer"         $ ContextsV2.txSignedBy info customerPkh                       -- customer must sign it
          ; let b = traceIfFalse "Incorrect Token payout"   $ isAddrGettingPaidExactly txOutputs customerAddr returnValue  -- customer gets token back
          ; let c = traceIfFalse "Incorrect Price payout"   $ isAddrGettingPaidExactly txOutputs printerAddr offerValue    -- printer gets paid
          ; let d = traceIfFalse "Single Script UTxO"       $ isNInputs txInputs 1 && isNOutputs contTxOutputs 0           -- single script input
          ; let e = traceIfFalse "Wrong Purchase Order"     $ Value.valueOf validatingValue purchaseOrderPid (siPOName sit) == (1 :: Integer)
          ;         traceIfFalse "CurrentlyShipping:Accept" $ all (==(True :: Bool)) [a,b,c,d,e]
          }
        -- other endpints fail
        _ -> False
  where
    info :: PlutusV2.TxInfo
    info = PlutusV2.scriptContextTxInfo context

    contTxOutputs :: [PlutusV2.TxOut]
    contTxOutputs = ContextsV2.getContinuingOutputs context

    txInputs :: [PlutusV2.TxInInfo]
    txInputs = PlutusV2.txInfoInputs  info

    txOutputs :: [PlutusV2.TxOut]
    txOutputs = PlutusV2.txInfoOutputs info

    -- token info
    validatingValue :: PlutusV2.Value
    validatingValue =
      case ContextsV2.findOwnInput context of
        Nothing    -> traceError "No Input to Validate." -- This error should never be hit.
        Just input -> PlutusV2.txOutValue $ PlutusV2.txInInfoResolved input
    
    -- datum stuff
    getContinuingDatum :: [PlutusV2.TxOut] -> PlutusV2.Value -> CustomDatumType
    getContinuingDatum []     _   = datum
    getContinuingDatum (x:xs) val =
      if PlutusV2.txOutValue x == val -- strict value continue
        then
          case PlutusV2.txOutDatum x of
            PlutusV2.NoOutputDatum       -> getContinuingDatum xs val -- datumless
            (PlutusV2.OutputDatumHash _) -> getContinuingDatum xs val -- embedded datum
            -- inline datum only
            (PlutusV2.OutputDatum (PlutusV2.Datum d)) -> 
              case PlutusTx.fromBuiltinData d of
                Nothing     -> getContinuingDatum xs val              -- bad data
                Just inline -> inline
        else getContinuingDatum xs val

    -- the validity range for the spending tx
    validityRange :: PlutusV2.POSIXTimeRange
    validityRange = PlutusV2.txInfoValidRange info
-------------------------------------------------------------------------------
-- | Now we need to compile the Validator.
-------------------------------------------------------------------------------
validator' :: PlutusV2.Validator
validator' = PlutusV2.mkValidatorScript
    $$(PlutusTx.compile [|| wrap ||])
 where
    wrap = Utils.mkUntypedValidator mkValidator
-------------------------------------------------------------------------------
-- | The code below is required for the plutus script compile.
-------------------------------------------------------------------------------
script :: Scripts.Script
script = Scripts.unValidatorScript validator'

printingPoolContractScriptShortBs :: SBS.ShortByteString
printingPoolContractScriptShortBs = SBS.toShort . LBS.toStrict $ serialise script

printingPoolContractScript :: PlutusScript PlutusScriptV2
printingPoolContractScript = PlutusScriptSerialised printingPoolContractScriptShortBs

