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
import qualified Plutus.V1.Ledger.Value         as Value
import qualified Plutus.V1.Ledger.Address       as Addr
import qualified Plutus.V2.Ledger.Contexts      as ContextsV2
import qualified Plutus.V2.Ledger.Api           as PlutusV2
import           Plutus.Script.Utils.V2.Scripts as Utils
import           UsefulFuncs
{-
  Author   : The Ancient Kraken
  Copyright: 2022
  Version  : Rev 1
-}
-------------------------------------------------------------------------------
-- | Starter Token Information
-------------------------------------------------------------------------------
-- starter policy id
startPid :: PlutusV2.CurrencySymbol
startPid = PlutusV2.CurrencySymbol { PlutusV2.unCurrencySymbol = createBuiltinByteString [246, 30, 28, 29, 56, 252, 78, 91, 7, 52, 50, 154, 75, 123, 130, 11, 118, 187, 142, 7, 41, 69, 140, 21, 60, 66, 72, 234] }
-------------------------------------------------------------------------------
-- | Marketplace Validator Hash
-------------------------------------------------------------------------------
marketValidatorHash :: PlutusV2.ValidatorHash
marketValidatorHash = PlutusV2.ValidatorHash $ createBuiltinByteString [70, 47, 31, 53, 30, 86, 236, 105, 131, 249, 214, 177, 53, 172, 125, 134, 9, 105, 85, 52, 63, 200, 163, 69, 113, 23, 42, 247]
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
  , mPrefixName  :: PlutusV2.BuiltinByteString
  -- ^ The purchase order Token Name Prefix.
  , mPoPrice     :: Integer
  -- ^ The purchase order price in lovelace.
  }
PlutusTx.unstableMakeIsData ''MarketDataType
-------------------------------------------------------------------------------
{-# INLINABLE mkPolicy #-}
mkPolicy :: BuiltinData -> PlutusV2.ScriptContext -> Bool
mkPolicy redeemer context = do
      { let a = traceIfFalse "No Starter NFT"  checkVal
      ; let b = traceIfFalse "Invoice Minting" checkMintedAmount
      ; let c = traceIfFalse "Script Spening"  $ checkInputs txInputs
      ;         traceIfFalse "Invoice Minting" $ all (==True) [a,b,c]
      }
  where
    info :: PlutusV2.TxInfo
    info = PlutusV2.scriptContextTxInfo context

    txInputs :: [PlutusV2.TxInInfo]
    txInputs = ContextsV2.txInfoInputs info

    redeemer' :: MarketDataType
    redeemer' = PlutusTx.unsafeFromBuiltinData @MarketDataType redeemer

    -- cont val
    scriptOutputs :: [(PlutusV2.OutputDatum, PlutusV2.Value)]
    scriptOutputs = ContextsV2.scriptOutputsAt marketValidatorHash info

    valueAtValidator :: Maybe PlutusV2.Value
    valueAtValidator = 
      if length scriptOutputs == 0
        then Nothing
        else Just $ snd $ head scriptOutputs

    -- sending back to the market and holding the starter nft
    checkVal :: Bool
    checkVal = 
      case valueAtValidator of
        Nothing              -> traceIfFalse "Marketplace Script Not Being Used" False
        Just validatingValue -> Value.valueOf validatingValue startPid (mStartName redeemer') == (1 :: Integer)

    -- minting an invoce nft
    checkMintedAmount :: Bool
    checkMintedAmount =
      case Value.flattenValue (PlutusV2.txInfoMint info) of
        [(cs, _, amt)] -> (cs == ContextsV2.ownCurrencySymbol context) && (amt == (1 :: Integer))
        _              -> traceIfFalse "Incorrect Minting Info" False


    -- spending from market and holding starter nft
    checkInputs :: [PlutusV2.TxInInfo] -> Bool
    checkInputs [] = False
    checkInputs (x:xs) =
      if PlutusV2.txOutAddress (PlutusV2.txInInfoResolved x) == Addr.scriptHashAddress marketValidatorHash
        then Value.valueOf (PlutusV2.txOutValue (PlutusV2.txInInfoResolved x)) startPid (mStartName redeemer') == (1 :: Integer)
        else checkInputs xs
    
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