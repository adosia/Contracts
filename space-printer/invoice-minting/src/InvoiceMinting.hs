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
{-# LANGUAGE NumericUnderscores    #-}
-- Options
{-# OPTIONS_GHC -fno-strictness               #-}
{-# OPTIONS_GHC -fno-ignore-interface-pragmas #-}
{-# OPTIONS_GHC -fno-omit-interface-pragmas   #-}
{-# OPTIONS_GHC -fobject-code                 #-}
{-# OPTIONS_GHC -fno-specialise               #-}
{-# OPTIONS_GHC -fexpose-all-unfoldings       #-}
module InvoiceMinting
  ( mintingPlutusScript
  , mintingScriptShortBs
  ) where
import qualified PlutusTx
import           PlutusTx.Prelude
import           Cardano.Api.Shelley            ( PlutusScript (..), PlutusScriptV2 )
import           Codec.Serialise                ( serialise )
import qualified Data.ByteString.Lazy           as LBS
import qualified Data.ByteString.Short          as SBS
import qualified Plutus.V1.Ledger.Scripts       as Scripts
import qualified Plutus.V1.Ledger.Address       as Addr
import qualified Plutus.V2.Ledger.Contexts      as ContextsV2
import qualified Plutus.V2.Ledger.Api           as PlutusV2
import           Plutus.Script.Utils.V2.Scripts as Utils
import           UsefulFuncs
{-
  Author   : The Ancient Kraken
  Copyright: 2022
-}
-------------------------------------------------------------------------------
-- | Starter Token Information
-------------------------------------------------------------------------------
-- starter policy id
startPid :: PlutusV2.CurrencySymbol
startPid = PlutusV2.CurrencySymbol { PlutusV2.unCurrencySymbol = createBuiltinByteString [236, 102, 139, 127, 238, 248, 107, 129, 145, 173, 190, 202, 215, 60, 58, 233, 79, 236, 26, 127, 247, 232, 146, 16, 181, 228, 45, 246] }
-------------------------------------------------------------------------------
-- | Marketplace Validator Hash
-------------------------------------------------------------------------------
marketValidatorHash :: PlutusV2.ValidatorHash
marketValidatorHash = PlutusV2.ValidatorHash $ createBuiltinByteString [103, 59, 36, 173, 185, 34, 28, 200, 202, 116, 227, 157, 120, 194, 96, 169, 57, 92, 242, 83, 86, 224, 183, 9, 20, 241, 79, 188]
-------------------------------------------------------------------------------
data MarketDataType = MarketDataType
  { mDesignerPKH :: PlutusV2.PubKeyHash
  -- ^ The Designer's payment public key hash.
  , mDesignerSC  :: PlutusV2.PubKeyHash
  -- ^ The Designer's staking credential.
  , mStartName   :: PlutusV2.TokenName
  -- ^ Starting Token Name for the designer.
  , mNumber      :: Integer
  -- ^ The current design increment number.
  , mPoPolicy    :: PlutusV2.CurrencySymbol
  -- ^ The purchase order Policy ID.
  , mPoPrice     :: Integer
  -- ^ The purchase order price in lovelace.
  , mPoFreeFlag  :: Integer
  -- ^ Allows a designer to indicate its a free mint.
  }
PlutusTx.unstableMakeIsData ''MarketDataType

checkDatumIncrease :: MarketDataType -> MarketDataType -> Bool
checkDatumIncrease a b =  ( mDesignerPKH a == mDesignerPKH b ) &&
                          ( mDesignerSC  a == mDesignerSC  b ) &&
                          ( mStartName   a == mStartName   b ) &&
                          ( mNumber  a + 1 == mNumber      b ) &&
                          ( mPoPolicy    a == mPoPolicy    b ) &&
                          ( mPoPrice     a == mPoPrice     b ) &&
                          ( mPoFreeFlag  a == mPoFreeFlag  b )
-------------------------------------------------------------------------------
{-# INLINABLE mkPolicy #-}
mkPolicy :: BuiltinData -> PlutusV2.ScriptContext -> Bool
mkPolicy _ context = traceIfFalse "Mint Error" checkMintDatum -- mint
  where
    info :: PlutusV2.TxInfo
    info = PlutusV2.scriptContextTxInfo context

    txInputs :: [PlutusV2.TxInInfo]
    txInputs = ContextsV2.txInfoInputs info

    -- check that the locking script has the correct datum hash
    checkMintDatum :: Bool
    checkMintDatum =
      case checkInputs txInputs of
        Nothing -> traceIfFalse "No Input Datum" False
        Just inputDatum  ->
          case datumAtValidator of
            Nothing          -> traceIfFalse "No Output Datum" False
            Just outputDatum -> checkDatumIncrease inputDatum outputDatum
    
    -- return the first datum hash from a txout going to the locking script
    checkInputs :: [PlutusV2.TxInInfo] -> Maybe MarketDataType
    checkInputs [] = Nothing
    checkInputs (x:xs) =
      if PlutusV2.txOutAddress (PlutusV2.txInInfoResolved x) == Addr.scriptHashAddress marketValidatorHash
        then getDatumFromTxOut $ PlutusV2.txInInfoResolved x
        else checkInputs xs

    -- check if the incoming datum is the correct form.
    getDatumFromTxOut :: PlutusV2.TxOut -> Maybe MarketDataType
    getDatumFromTxOut x = 
      case PlutusV2.txOutDatum x of
        PlutusV2.NoOutputDatum       -> Nothing -- datumless
        (PlutusV2.OutputDatumHash _) -> Nothing -- embedded datum
        -- inline datum
        (PlutusV2.OutputDatum (PlutusV2.Datum d)) -> 
          case PlutusTx.fromBuiltinData d of
            Nothing     -> Nothing
            Just inline -> Just $ PlutusTx.unsafeFromBuiltinData @MarketDataType inline

    datumAtValidator :: Maybe MarketDataType
    datumAtValidator =
      if length scriptOutputs == 0 
        then Nothing
        else 
          let datumAtValidator' = fst $ head scriptOutputs
          in case datumAtValidator' of
            PlutusV2.NoOutputDatum       -> Nothing -- datumless
            (PlutusV2.OutputDatumHash _) -> Nothing -- embedded datum
            -- inline datum
            (PlutusV2.OutputDatum (PlutusV2.Datum d)) -> 
              case PlutusTx.fromBuiltinData d of
                Nothing     -> Nothing
                Just inline -> Just $ PlutusTx.unsafeFromBuiltinData @MarketDataType inline
      where 
        scriptOutputs :: [(PlutusV2.OutputDatum, PlutusV2.Value)]
        scriptOutputs = ContextsV2.scriptOutputsAt marketValidatorHash info
-------------------------------------------------------------------------------
policy :: PlutusV2.MintingPolicy
policy = PlutusV2.mkMintingPolicyScript $$(PlutusTx.compile [|| wrap ||])
  where
    wrap = Utils.mkUntypedMintingPolicy mkPolicy

plutusScript :: Scripts.Script
plutusScript = PlutusV2.unMintingPolicyScript policy

validator :: PlutusV2.Validator
validator = PlutusV2.Validator plutusScript

scriptAsCbor :: LBS.ByteString
scriptAsCbor = serialise validator

mintingPlutusScript :: PlutusScript PlutusScriptV2
mintingPlutusScript = PlutusScriptSerialised . SBS.toShort $ LBS.toStrict scriptAsCbor

mintingScriptShortBs :: SBS.ShortByteString
mintingScriptShortBs = SBS.toShort . LBS.toStrict $ scriptAsCbor