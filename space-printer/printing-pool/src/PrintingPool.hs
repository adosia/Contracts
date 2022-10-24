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
import           DataTypes
import           UsefulFuncs
{- |
  Author   : The Ancient Kraken
  Copyright: 2022
  Version  : Rev 1
-}
-------------------------------------------------------------------------------
-- | Create the datum parameters data object.
-------------------------------------------------------------------------------
data CustomDatumType =  PrintingPool      PrintingPoolType     |
                        OfferInformation  OfferInformationType |
                        CurrentlyShipping ShippingInfoType
PlutusTx.makeIsDataIndexed ''CustomDatumType  [ ('PrintingPool,        0)
                                              , ('OfferInformation,    1)
                                              , ('CurrentlyShipping,   2)
                                              ]
-------------------------------------------------------------------------------
-- | Create the redeemer parameters data object.
-------------------------------------------------------------------------------
data CustomRedeemerType = Remove |
                          Update |
                          Offer  |
                          Accept |
                          Cancel |
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
    (PrintingPool ppt) ->
      case redeemer of
        Remove -> True -- do
          -- { let customerPkh  = ppCustomerPKH ppt
          -- ; let customerSc   = ppCustomerSC  ppt
          -- ; let customerAddr = createAddress customerPkh customerSc
          -- ; let a = traceIfFalse "Incorrect Signer"       $ txSignedBy info customerPkh                                     -- customer must sign it
          -- ; let b = traceIfFalse "Incorrect Token Return" $ checkTxOutForValueAtAddr txOutputs customerAddr validatingValue -- token must go back to customer
          -- ; let c = traceIfFalse "Too Many Script Inputs" $ checkForNScriptInputs txInputs (1 :: Integer)                   -- single script input
          -- ; all (==(True :: Bool)) [a,b,c]
          -- }
        Update -> True
          -- case embeddedDatum continuingTxOutputs of
          --   (PrintingPool ppt') -> do
          --     { let a = traceIfFalse "Incorrect Signer"       $ txSignedBy info (ppCustomerPKH ppt)                        -- customer must sign it
          --     ; let b = traceIfFalse "Incorrect Token Return" $ checkContTxOutForValue continuingTxOutputs validatingValue -- token must go back to script
          --     ; let c = traceIfFalse "Too Many Script Inputs" $ checkForNScriptInputs txInputs (1 :: Integer)              -- single script input
          --     ; let d = traceIfFalse "Incorrect New State"    $ ppt == ppt'                                                -- change region codes only
          --     ; let e = traceIfFalse "Wrong Purchase Order"   $ Value.valueOf validatingValue (poPolicyId ppp) (ppPOName ppt) == (1 :: Integer)
          --     ; all (==(True :: Bool)) [a,b,c,d,e]
          --     }
          --   _ -> False
        Offer -> True 
          -- case embeddedDatum continuingTxOutputs of
          --   (OfferInformation oit) -> do
          --     { let newValue = validatingValue + Ada.lovelaceValueOf (oiOfferPrice oit)
          --     ; let a = traceIfFalse "Incorrect Signer"      $ txSignedBy info (ppCustomerPKH ppt) && txSignedBy info (oiPrinterPKH oit) -- The printer must sign it
          --     ; let b = traceIfFalse "Offer: Incrt Tkn Cont" $ checkContTxOutForValue continuingTxOutputs newValue                       -- token must go back to script
          --     ; let c = traceIfFalse "Incorrect New State"   $ ppt === oit                                                               -- printer must change the datum 
          --     ; let d = traceIfFalse "Wrong Purchase Order"  $ Value.valueOf validatingValue (poPolicyId ppp) (ppPOName ppt) == (1 :: Integer)
          --     ; all (==(True :: Bool)) [a,b,c,d]
          --     }
          --   _ -> False
        _ -> False
    (OfferInformation oit) ->
      case redeemer of
        Cancel -> True
          -- case embeddedDatum continuingTxOutputs of
          --   (PrintingPool ppt) -> do
          --     { let customerPkh  = ppCustomerPKH ppt
          --     ; let customerSc   = ppCustomerSC  ppt
          --     ; let customerAddr = createAddress customerPkh customerSc
          --     ; let offerValue = Ada.lovelaceValueOf (oiOfferPrice oit)
          --     ; let newValue = validatingValue - offerValue
          --     ; let a = traceIfFalse "Incorrect State"      $ ppt === oit                                                -- the token is getting an offer
          --     ; let b = traceIfFalse "Incorrect Signer"     $ txSignedBy info (oiPrinterPKH oit)                         -- customer must sign it
          --     ; let c = traceIfFalse "Incorrect Token Cont" $ checkContTxOutForValue continuingTxOutputs newValue        -- token must go back to script
          --     ; let d = traceIfFalse "Incorrect Token Chan" $ checkTxOutForValueAtAddr txOutputs customerAddr offerValue -- token must go back to customer
          --     ; let e = traceIfFalse "SingleScript Inputs"  $ checkForNScriptInputs txInputs (1 :: Integer)              -- single script input
          --     ; let f = traceIfFalse "Wrong Purchase Order" $ Value.valueOf validatingValue (poPolicyId ppp) (oiPOName oit) == (1 :: Integer)
          --     ; all (==(True :: Bool)) [a,b,c,d,e,f]
          --     }
          --   _ -> False
        Remove -> True
          -- case embeddedDatum continuingTxOutputs of
          --   (PrintingPool ppt) -> do
          --     { let customerPkh  = ppCustomerPKH ppt
          --     ; let customerSc   = ppCustomerSC  ppt
          --     ; let customerAddr = createAddress customerPkh customerSc
          --     ; let offerValue = Ada.lovelaceValueOf (oiOfferPrice oit)
          --     ; let newValue = validatingValue - offerValue 
          --     ; let a = traceIfFalse "Incorrect State"       $ ppt === oit                                                -- the token is getting an offer
          --     ; let b = traceIfFalse "Incorrect Signer"      $ txSignedBy info (oiCustomerPKH oit)                        -- customer must sign it
          --     ; let c = traceIfFalse "Incorrect Token Cont"  $ checkContTxOutForValue continuingTxOutputs newValue        -- token must go back to script
          --     ; let d = traceIfFalse "Incorrect Token Chan"  $ checkTxOutForValueAtAddr txOutputs customerAddr offerValue -- token must go back to customer
          --     ; let e = traceIfFalse "SingleScript Inputs"   $ checkForNScriptInputs txInputs (1 :: Integer)              -- single script input
          --     ; let f = traceIfFalse "Job Is Still Printing" $ not $ overlaps (lockInterval (oiPrintTime oit)) validityRange
          --     ; let g = traceIfFalse "Wrong Purchase Order"  $ Value.valueOf validatingValue (poPolicyId ppp) (oiPOName oit) == (1 :: Integer)
          --     ; all (==(True :: Bool)) [a,b,c,d,e,f,g]
          --     }
          --   _ -> False
        Ship -> True
          -- case embeddedDatum continuingTxOutputs of
          --   (CurrentlyShipping sit) -> do
          --     { let a = traceIfFalse "Incorrect Signer"     $ txSignedBy info (oiPrinterPKH oit)                         -- printer must sign
          --     ; let b = traceIfFalse "Incorrect Token Cont" $ checkContTxOutForValue continuingTxOutputs validatingValue -- token must go back to script
          --     ; let c = traceIfFalse "Incorrect New State"  $ oit === sit                                                -- job is done printing and is now shipping
          --     ; let d = traceIfFalse "SingleScript Inputs"  $ checkForNScriptInputs txInputs (1 :: Integer)              -- single script input
          --     ; let e = traceIfFalse "Wrong Purchase Order" $ Value.valueOf validatingValue (poPolicyId ppp) (oiPOName oit) == (1 :: Integer)
          --     ; all (==(True :: Bool)) [a,b,c,d,e]
          --     }
          --   _ -> False
        _ -> False
    (CurrentlyShipping sit) ->
      case redeemer of
        Accept -> True -- do
        --   { let customerPkh  = siCustomerPKH sit
        --   ; let customerSc   = siCustomerSC  sit
        --   ; let customerAddr = createAddress customerPkh customerSc
        --   ; let printerPkh   = siPrinterPKH sit
        --   ; let printerSc    = siPrinterSC  sit
        --   ; let printerAddr  = createAddress printerPkh printerSc
        --   ; let offerValue   = Ada.lovelaceValueOf (siOfferPrice sit)
        --   ; let newValue     = validatingValue - offerValue 
        --   ; let a = traceIfFalse "Incorrect Signer"       $ txSignedBy info customerPkh                               -- customer must sign it
        --   ; let b = traceIfFalse "Incorrect Token payout" $ checkTxOutForValueAtAddr txOutputs customerAddr newValue  -- customer gets token back
        --   ; let c = traceIfFalse "Incorrect Price payout" $ checkTxOutForValueAtAddr txOutputs printerAddr offerValue -- printer gets paid
        --   ; let d = traceIfFalse "Single Script Inputs"   $ checkForNScriptInputs txInputs (1 :: Integer)             -- single script input
        --   ; let e = traceIfFalse "Wrong Purchase Order"   $ Value.valueOf validatingValue (poPolicyId ppp) (siPOName sit) == (1 :: Integer)
        --   ; all (==(True :: Bool)) [a,b,c,d,e]
        --   }
        -- _ -> False
  -- where
  --   info :: TxInfo
  --   info = scriptContextTxInfo context

  --   continuingTxOutputs  :: [TxOut]
  --   continuingTxOutputs  = getContinuingOutputs context

  --   txInputs :: [TxInInfo]
  --   txInputs = txInfoInputs info

  --   txOutputs :: [TxOut]
  --   txOutputs = txInfoOutputs info

  --   validatingValue :: Value
  --   validatingValue =
  --     case findOwnInput context of
  --       Nothing     -> traceError "No Input to Validate."  -- This should never be hit.
  --       Just input  -> txOutValue $ txInInfoResolved input
    
  --   embeddedDatum :: [TxOut] -> CustomDatumType
  --   embeddedDatum [] = datum
  --   embeddedDatum (txOut:txOuts) =
  --     case txOutDatumHash txOut of
  --       Nothing -> embeddedDatum txOuts
  --       Just dh ->
  --         case findDatum dh info of
  --           Nothing        -> embeddedDatum txOuts
  --           Just (Datum datum') -> Data.Maybe.fromMaybe datum (PlutusTx.fromBuiltinData datum')

  --   validityRange :: POSIXTimeRange
  --   validityRange = txInfoValidRange info
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

