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
  ( apiExamplePlutusMintingScript
  , mintingScriptShortBs
  ) where

import qualified PlutusTx
import           Codec.Serialise
import           Ledger                     hiding ( singleton )
import           PlutusTx.Prelude           hiding ( Semigroup (..), unless )
import           Cardano.Api.Shelley        ( PlutusScript (..), PlutusScriptV1 )
import           Plutus.V1.Ledger.Value     as Value
import qualified Plutus.V1.Ledger.Address   as Address
import qualified Data.ByteString.Lazy       as LB
import qualified Data.ByteString.Short      as SBS
import qualified Ledger.Typed.Scripts       as Scripts
-- import qualified PlutusTx.Builtins.Internal as Internal
import           Data.Aeson                 ( FromJSON, ToJSON )
import           Data.OpenApi.Schema        ( ToSchema )
import           GHC.Generics               ( Generic )
import           Prelude                    ( Show )

import TokenHelper
{-
  Author   : The Ancient Kraken
  Copyright: 2022
  Version  : Rev 0
-}
newtype MintParams = MintParams
  { mpValidatorHash :: ValidatorHash
  }
PlutusTx.makeLift ''MintParams

data CustomRedeemerType = CustomRedeemerType
  { mDesignerPKH :: !PubKeyHash
  , mDesignerSC  :: !PubKeyHash
  , mStartPolicy :: !CurrencySymbol
  , mStartName   :: !TokenName
  , mNumber      :: !Integer
  , mPoPolicy    :: !CurrencySymbol
  , mPrefixName  :: !BuiltinByteString
  , mPoPrice     :: !Integer
  }
    deriving stock    ( Show, Generic )
    deriving anyclass ( FromJSON, ToJSON, ToSchema )
PlutusTx.unstableMakeIsData ''CustomRedeemerType
PlutusTx.makeLift ''CustomRedeemerType

-------------------------------------------------------------------------------
{-# INLINABLE mkPolicy #-}
mkPolicy :: MintParams -> BuiltinData -> ScriptContext -> Bool
mkPolicy mp redeemer context = do
      { let a = traceIfFalse "Cont Must Hold NFT Error"  checkVal
      ; let b = traceIfFalse "FT Minting Error"          checkMintedAmount
      ; let c = traceIfFalse "Script Input Datum Error"  checkInputDatum
      ; let d = traceIfFalse "Script Output Datum Error" checkOutputDatum
      ;         traceIfFalse "Minting FT Endpoint Error" $ all (==True) [a,b,c,d]
      }
  where

    info :: TxInfo
    info = scriptContextTxInfo context

    txInputs :: [TxInInfo]
    txInputs = txInfoInputs info

    checkInputs :: [TxInInfo] -> Maybe DatumHash
    checkInputs [] = Nothing
    checkInputs (x:xs) =
      if txOutAddress (txInInfoResolved x) == Address.scriptHashAddress (mpValidatorHash mp)
        then txOutDatumHash (txInInfoResolved x)
        else checkInputs xs

    checkInputDatum :: Bool
    checkInputDatum =
      case checkInputs txInputs of
        Nothing -> traceIfFalse "No Input Datum Hash" False
        Just dh ->
          case findDatumHash (Datum $ PlutusTx.toBuiltinData d) info of
            Nothing -> traceIfFalse "Incorrect Input Datum Hash" False
            Just dh' -> dh == dh'
      where
        d :: CustomRedeemerType
        d = CustomRedeemerType
              { mDesignerPKH = mDesignerPKH redeemer'
              , mDesignerSC  = mDesignerSC  redeemer'
              , mStartPolicy = mStartPolicy redeemer'
              , mStartName   = mStartName   redeemer'
              , mNumber      = mNumber      redeemer'
              , mPoPolicy    = mPoPolicy    redeemer'
              , mPrefixName  = mPrefixName  redeemer'
              , mPoPrice     = mPoPrice     redeemer'
              }

    valueAtValidator :: Value
    valueAtValidator = snd $ head $ scriptOutputsAt (mpValidatorHash mp) info

    checkVal :: Bool
    checkVal = traceIfFalse "Incorrect Script Amount" $ Value.valueOf valueAtValidator (mStartPolicy redeemer') (mStartName redeemer') == (1 :: Integer)

    datumHashAtValidator :: DatumHash
    datumHashAtValidator = fst $ head $ scriptOutputsAt (mpValidatorHash mp) info

    checkOutputDatum :: Bool
    checkOutputDatum =
      case findDatumHash (Datum $ PlutusTx.toBuiltinData d) info of
        Nothing -> traceIfFalse "Incorrect Outbound Datum Hash" False
        Just dh -> dh == datumHashAtValidator
      where
        d :: CustomRedeemerType
        d = CustomRedeemerType
              { mDesignerPKH = mDesignerPKH redeemer'
              , mDesignerSC  = mDesignerSC  redeemer'
              , mStartPolicy = mStartPolicy redeemer'
              , mStartName   = mStartName   redeemer'
              , mNumber      = mNumber      redeemer' + 1
              , mPoPolicy    = mPoPolicy    redeemer'
              , mPrefixName  = mPrefixName  redeemer'
              , mPoPrice     = mPoPrice     redeemer'
              }

    redeemer' :: CustomRedeemerType
    redeemer' = PlutusTx.unsafeFromBuiltinData @CustomRedeemerType redeemer

    checkPolicyId :: CurrencySymbol ->  Bool
    checkPolicyId cs = traceIfFalse "Incorrect Policy Id" $ cs == ownCurrencySymbol context

    checkAmount :: Integer -> Bool
    checkAmount amt = traceIfFalse "Incorrect Mint Amount" $ amt == (1 :: Integer)

    checkTkn :: TokenName -> Bool
    checkTkn tkn = traceIfFalse "Incorrect Token Name" $ Value.unTokenName tkn == nftName (mPrefixName redeemer') (mNumber redeemer')

    checkMintedAmount :: Bool
    checkMintedAmount =
      case Value.flattenValue (txInfoMint info) of
        [(cs, tkn, amt)] -> checkPolicyId cs && checkTkn tkn && checkAmount amt
        _                -> traceIfFalse "Mint/Burn Error" False
    

-------------------------------------------------------------------------------
policy :: MintParams -> Scripts.MintingPolicy
policy mp = mkMintingPolicyScript ($$(PlutusTx.compile [|| Scripts.wrapMintingPolicy . mkPolicy ||]) `PlutusTx.applyCode` PlutusTx.liftCode mp)
-------------------------------------------------------------------------------
plutusScript :: Script
plutusScript = unMintingPolicyScript $ policy params
  where
    params = MintParams { mpValidatorHash = "6e3e16c486efbac98aaa646510f31e794e544622f745fb339691274e" }

validator :: Validator
validator = Validator plutusScript

-------------------------------------------------------------------------------
-- Do Not Remove
scriptAsCbor :: LB.ByteString
scriptAsCbor = serialise validator

apiExamplePlutusMintingScript :: PlutusScript PlutusScriptV1
apiExamplePlutusMintingScript = PlutusScriptSerialised . SBS.toShort $ LB.toStrict scriptAsCbor

mintingScriptShortBs :: SBS.ShortByteString
mintingScriptShortBs = SBS.toShort . LB.toStrict $ scriptAsCbor