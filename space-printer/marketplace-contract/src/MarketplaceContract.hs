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
module MarketplaceContract
  ( lockingContractScript
  , lockingContractScriptShortBs
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
import           TokenHelper
{- |
  Author   : The Ancient Kraken
  Copyright: 2022
  Version  : Rev 1
-}
-------------------------------------------------------------------------------
-- | Create the redeemer type.
-------------------------------------------------------------------------------
data CustomRedeemerType = MintPO |
                          Remove
PlutusTx.makeIsDataIndexed ''CustomRedeemerType [ ( 'MintPO, 0 )
                                                , ( 'Remove, 1 )
                                                ]
-------------------------------------------------------------------------------
-- | mkValidator :: Datum -> Redeemer -> ScriptContext -> Bool
-------------------------------------------------------------------------------
{-# INLINABLE mkValidator #-}
mkValidator :: MarketDataType -> CustomRedeemerType -> PlutusV2.ScriptContext -> Bool
mkValidator datum redeemer context =
  case redeemer of
    MintPO -> do
      { let designerPkh   = mDesignerPKH datum
      ; let designerSc    = mDesignerSC  datum
      ; let designerAddr  = createAddress designerPkh designerSc
      ; let a = traceIfFalse "Starting NFT"    $ Value.valueOf validatingValue (mStartPolicy datum) (mStartName datum) == (1 :: Integer)
      ; let b = traceIfFalse "Single Script"   $ isNInputs txInputs 1 && isNOutputs contTxOutputs 1
      ; let c = traceIfFalse "Designer Payout" $ isAddrGettingPaidExactly txOutputs designerAddr (adaValue $ mPoPrice datum)
      ; let d = traceIfFalse "Minting Info"    checkMintedAmount
      ; let e = traceIfFalse "In/Out Datum"    $ isEmbeddedDatumIncreasing contTxOutputs
      ;         traceIfFalse "MintPO Endpoint" $ all (==True) [a,b,c,d,e]
      }
    Remove -> do
      { let designerPkh  = mDesignerPKH datum
      ; let designerSc   = mDesignerSC  datum
      ; let designerAddr = createAddress designerPkh designerSc
      ; let a = traceIfFalse "Incorrect Signer"   $ ContextsV2.txSignedBy info designerPkh
      ; let b = traceIfFalse "Single Script UTxO" $ isNInputs txInputs 1 && isNOutputs contTxOutputs 0 
      ; let c = traceIfFalse "Value Not Returned" $ isAddrGettingPaidExactly txOutputs designerAddr validatingValue
      ;         traceIfFalse "Remove Endpoint"    $ all (==True) [a,b,c]
      }
   where
    info :: PlutusV2.TxInfo
    info = PlutusV2.scriptContextTxInfo  context

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
    
    -- minting stuff
    checkMintedAmount :: Bool
    checkMintedAmount =
      case Value.flattenValue (PlutusV2.txInfoMint info) of
        [(cs, tkn, amt)] -> (cs == mPoPolicy datum) && ((Value.unTokenName tkn) == nftName (mPrefixName datum) (mNumber datum)) && (amt == (1 :: Integer))
        _                -> traceIfFalse "Incorrect Minting Info" False
    
    -- datum stuff
    isEmbeddedDatumIncreasing :: [PlutusV2.TxOut] -> Bool
    isEmbeddedDatumIncreasing []     = traceIfFalse "No Increasing Datum Found" False
    isEmbeddedDatumIncreasing (x:xs) =
      if PlutusV2.txOutValue x == validatingValue -- strict value continue
        then
          case PlutusV2.txOutDatum x of
            PlutusV2.NoOutputDatum       -> isEmbeddedDatumIncreasing xs -- datumless
            (PlutusV2.OutputDatumHash _) -> isEmbeddedDatumIncreasing xs -- embedded datum
            -- inline datum only
            (PlutusV2.OutputDatum (PlutusV2.Datum d)) -> 
              case PlutusTx.fromBuiltinData d of
                Nothing     -> isEmbeddedDatumIncreasing xs              -- bad data
                Just inline -> traceIfFalse "Datum equality failure" $ checkDatumIncrease datum inline
        else isEmbeddedDatumIncreasing xs
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

lockingContractScriptShortBs :: SBS.ShortByteString
lockingContractScriptShortBs = SBS.toShort . LBS.toStrict $ serialise script

lockingContractScript :: PlutusScript PlutusScriptV2
lockingContractScript = PlutusScriptSerialised lockingContractScriptShortBs