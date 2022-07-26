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
  , Schema
  , contract
  ) where
import qualified PlutusTx
import           PlutusTx.Prelude
import           Plutus.Contract
import           Codec.Serialise            ( serialise )
import qualified Data.ByteString.Lazy       as LBS
import qualified Data.ByteString.Short      as SBS
import           Ledger                     hiding ( singleton )
import qualified Ledger.Typed.Scripts       as Scripts
import qualified Plutus.V1.Ledger.Scripts   as Plutus
import qualified Plutus.V1.Ledger.Value     as Value
import qualified Plutus.V1.Ledger.Ada       as Ada
import           Cardano.Api.Shelley        ( PlutusScript (..), PlutusScriptV1 )
import           CheckFuncs
import           DataTypes
import           TokenHelper
{- |
  Author   : The Ancient Kraken
  Copyright: 2022
  Version  : Rev 0
-}
-------------------------------------------------------------------------------
-- | Create the contract parameters data object.
-------------------------------------------------------------------------------
data LockingContractParams = LockingContractParams {}
PlutusTx.makeLift ''LockingContractParams

-------------------------------------------------------------------------------
-- | Create the redeemer type.
-------------------------------------------------------------------------------
data CustomRedeemerType = MintPO |
                          Remove
PlutusTx.makeIsDataIndexed ''CustomRedeemerType [ ( 'MintPO, 0 )
                                                , ( 'Remove, 1 )
                                                ]
PlutusTx.makeLift ''CustomRedeemerType

-------------------------------------------------------------------------------
-- | mkValidator :: TypeData -> Datum -> Redeemer -> ScriptContext -> Bool
-------------------------------------------------------------------------------
{-# INLINABLE mkValidator #-}
mkValidator :: LockingContractParams -> MarketDataType -> CustomRedeemerType -> ScriptContext -> Bool
mkValidator _ datum redeemer context =
  case redeemer of
    MintPO -> do
      { let designerPkh   = mDesignerPKH datum
      ; let designerSc    = mDesignerSC  datum
      ; let designerAddr  = createAddress designerPkh designerSc
      ; let a = traceIfFalse "Starting NFT Error"    $ Value.valueOf validatingValue (mStartPolicy datum) (mStartName datum) == (1 :: Integer)
      ; let b = traceIfFalse "Single Script Error"   $ isSingleScript txInputs
      ; let c = traceIfFalse "Cont Value Error"      $ isValueContinuing contOutputs validatingValue
      ; let d = traceIfFalse "Minting Info Error"    checkMintedAmount
      ; let e = traceIfFalse "In/Out Datum Error"    $ isEmbeddedDatum contOutputs
      ; let f = traceIfFalse "Designer Payout Error" $ isAddrGettingPaid txOutputs designerAddr (Ada.lovelaceValueOf $ mPoPrice datum)
      ;         traceIfFalse "MintPO Endpoint Error" $ all (==True) [a,b,c,d,e,f]
      }
    Remove -> do
      { let designerPkh   = mDesignerPKH datum
      ; let designerSc    = mDesignerSC  datum
      ; let designerAddr  = createAddress designerPkh designerSc
      ; let a = traceIfFalse "Incorrect Signer Error" $ txSignedBy info (mDesignerPKH datum)
      ; let b = traceIfFalse "Single Script Error"    $ isSingleScript txInputs
      ; let c = traceIfFalse "Designer Pay Error"     $ isAddrGettingPaid txOutputs designerAddr validatingValue
      ;         traceIfFalse "Remove Endpoint Error"  $ all (==True) [a,b,c]
      }
   where
    info :: TxInfo
    info = scriptContextTxInfo  context

    contOutputs :: [TxOut]
    contOutputs = getContinuingOutputs context

    txInputs :: [TxInInfo]
    txInputs = txInfoInputs  info

    txOutputs :: [TxOut]
    txOutputs = txInfoOutputs info

    validatingValue :: Value
    validatingValue =
      case findOwnInput context of
        Nothing    -> traceError "No Input to Validate." -- This error should never be hit.
        Just input -> txOutValue $ txInInfoResolved input

    checkMintedAmount :: Bool
    checkMintedAmount =
      case Value.flattenValue (txInfoMint info) of
        [(cs, tkn, amt)] -> (cs == mPoPolicy datum) && (Value.unTokenName tkn == nftName (mPrefixName datum) (mNumber datum)) && (amt == 1)
        _                -> False

    isEmbeddedDatum :: [TxOut] -> Bool
    isEmbeddedDatum []     = False
    isEmbeddedDatum (x:xs) =
      case txOutDatumHash x of
        Nothing -> isEmbeddedDatum xs
        Just dh ->
          case findDatum dh info of
            Nothing        -> False
            Just (Datum d) ->
              case PlutusTx.fromBuiltinData d of
                Nothing       -> False
                Just embedded -> datum == embedded

-------------------------------------------------------------------------------
-- | This determines the data type for Datum and Redeemer.
-------------------------------------------------------------------------------
data Typed
instance Scripts.ValidatorTypes Typed where
  type instance DatumType    Typed = MarketDataType
  type instance RedeemerType Typed = CustomRedeemerType

-------------------------------------------------------------------------------
-- | Now we need to compile the Typed Validator.
-------------------------------------------------------------------------------
typedValidator :: LockingContractParams -> Scripts.TypedValidator Typed
typedValidator lcp = Scripts.mkTypedValidator @Typed
  ($$(PlutusTx.compile [|| mkValidator ||]) `PlutusTx.applyCode` PlutusTx.liftCode lcp)
   $$(PlutusTx.compile [|| wrap        ||])
    where
      wrap = Scripts.wrapValidator @MarketDataType @CustomRedeemerType  -- @Datum @Redeemer

-------------------------------------------------------------------------------
-- | Define The Validator Here
-------------------------------------------------------------------------------
validator :: Plutus.Validator
validator = Scripts.validatorScript (typedValidator $ LockingContractParams {})

-------------------------------------------------------------------------------
-- | The code below is required for the plutus script compile.
-------------------------------------------------------------------------------
script :: Plutus.Script
script = Plutus.unValidatorScript validator

lockingContractScriptShortBs :: SBS.ShortByteString
lockingContractScriptShortBs = SBS.toShort . LBS.toStrict $ serialise script

lockingContractScript :: PlutusScript PlutusScriptV1
lockingContractScript = PlutusScriptSerialised lockingContractScriptShortBs
-------------------------------------------------------------------------------
-- | Off Chain
-------------------------------------------------------------------------------
type Schema = Endpoint "" ()

contract :: AsContractError e => Contract () Schema e ()
contract = selectList [] >> contract