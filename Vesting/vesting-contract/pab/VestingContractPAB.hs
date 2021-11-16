{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE DerivingVia                #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImportQualifiedPost        #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}


-- Default PAB stuff
import           Control.Monad        (void)
import           Ledger               (Address, ScriptContext)
import qualified Ledger.Constraints   as Constraints
import qualified Ledger.Scripts
import           Ledger.Value         (Value)
import           Plutus.Contract
import qualified PlutusTx
import Playground.Contract
import           PlutusTx.Prelude     hiding (Applicative (..))
import qualified Prelude             as Haskell
import           Data.Map             as Map
import           Data.Void            (Void)
import Plutus.ChainIndex.Tx
import           Data.Aeson                (ToJSON, FromJSON)
import           GHC.Generics              (Generic)
import Plutus.V1.Ledger.Scripts (Datum (..), DatumHash, MintingPolicyHash, Redeemer, ValidatorHash)

-- module VestingContract
--   ( vestingContractScript
--   , vestingContractScriptShortBs
--   ) where

-- Non-PAB Imports
import           Codec.Serialise

import qualified Data.ByteString.Lazy      as LBS
import qualified Data.ByteString.Short     as SBS

import           Prelude                   hiding (($))

import           Ledger                    hiding (singleton)
import qualified Ledger.Interval
import qualified Ledger.TimeSlot
import qualified Ledger.Typed.Scripts      as Scripts

import           Cardano.Api.Shelley       (PlutusScript (..), PlutusScriptV1)

import qualified PlutusTx
import           PlutusTx.Prelude          as P hiding (Semigroup (..), unless)

import qualified Plutus.V1.Ledger.Ada      as Ada
import qualified Plutus.V1.Ledger.Contexts as Contexts
import qualified Plutus.V1.Ledger.Scripts  as Plutus
import qualified Plutus.V1.Ledger.Value    as Value
import           Plutus.V1.Ledger.Time     as Time

{- |
  Author   : The Ancient Kraken
  Copyright: 2021
  Version  : Rev 0

  This is a vesting contract that attempts to solve the permanent banning problem.
-}

-------------------------------------------------------------------------------
-- | Create the token sale parameters data object.
-------------------------------------------------------------------------------
data VestingContractParams = VestingContractParams
  { vcMajorityParam :: !Integer
  -- ^ The number of keys that determines the majority.
  , vcPolicyID      :: !CurrencySymbol
  -- ^ The policy id of the vesting token.
  , vcTokenName     :: !TokenName
  -- ^ The token name of the vesting token.
  }
PlutusTx.makeLift ''VestingContractParams

vestingContractParams = VestingContractParams
  { vcMajorityParam = 3         -- | This may need to be inside the datum
  , vcTokenName     = "TokenC"  -- | sample tn
  , vcPolicyID      = "5243f6530c3507a3ed1217848475abb5ec0ec122e00c82e878ff2292"  -- | sample pid
  }

-------------------------------------------------------------------------------
-- | Create the datum parameters data object.
-------------------------------------------------------------------------------

data CustomDatumType = CustomDatumType 
  { cdtVestAmount      :: !Integer
  -- ^ The amount of lovelace the user is allowed to extract.
  , cdtVestDeadline    :: !Integer
  -- ^ Fund must be retrieved after this deadline.
  , cdtVestingUserPKH  :: !PubKeyHash
  -- ^ The public key hash of the receiver.
  , cdtVestingGroupPKH :: ![PubKeyHash]
  -- ^ A list public key hashes of everyone who is vesting with the contract.
  , cdtTreasuryPKH     :: !PubKeyHash
  -- ^ The public key hash of the treasury wallet.
  }
PlutusTx.unstableMakeIsData ''CustomDatumType
PlutusTx.makeLift ''CustomDatumType


-------------------------------------------------------------------------------
-- | Create the redeemer parameters data object.
-------------------------------------------------------------------------------

data CustomRedeemerType = Redeem | Refund
    deriving (Generic)
    deriving anyclass (FromJSON, ToJSON, ToSchema)
PlutusTx.unstableMakeIsData ''CustomRedeemerType
PlutusTx.makeLift ''CustomRedeemerType


-------------------------------------------------------------------------------
-- | Define The Token Sale Parameters Here
-------------------------------------------------------------------------------

validator :: Plutus.Validator
validator = Scripts.validatorScript (typedValidator vestingContractParams)


-------------------------------------------------------------------------------
-- | mkValidator :: Data -> Datum -> Redeemer -> ScriptContext -> Bool
-------------------------------------------------------------------------------
{-# INLINABLE mkValidator #-}
mkValidator :: VestingContractParams -> CustomDatumType -> CustomRedeemerType -> ScriptContext -> Bool
mkValidator vc datum redeemer context =
  case redeemer of
      Redeem -> traceIfFalse "Token Redeem Has Failed"  $ redeem
      Refund -> traceIfFalse "Token Removal Has Failed" $ refund
    where
      -------------------------------------------------------------------------
      -- | Different Types of Validators Here
      -------------------------------------------------------------------------
      
      -- | Put all the retrieve functions together here.
      redeem :: Bool
      redeem = do
        { let a = checkTxSigner vestingPKH
        ; let b = checkTxOutForValueAtAddress currentTxOutputs vestingAddr vestingValue
        ; let c = checkTxOutForValue scriptTxOutputs returnValue
        ; let d = (Ledger.TimeSlot.slotToBeginPOSIXTime def (Slot $ cdtVestDeadline datum)) `before` Contexts.txInfoValidRange info
        ; P.all (P.==(True :: Bool)) [a,b,c,d]
        }
      
      -- | Put all the retrieve functions together here.
      refund :: Bool
      refund = do
        { let a = True
        ; let b = True
        ; P.all (P.==(True :: Bool)) [a,b]
        }

      -------------------------------------------------------------------------
      -- | Script Info and TxOutputs
      -------------------------------------------------------------------------
      
      -- The script info.
      info :: TxInfo
      info = scriptContextTxInfo context

      -- All the current outputs.
      currentTxOutputs :: [TxOut]
      currentTxOutputs = txInfoOutputs info

      -- All the outputs going back to the script.
      scriptTxOutputs  :: [TxOut]
      scriptTxOutputs  = Contexts.getContinuingOutputs context

      -------------------------------------------------------------------------
      -- | Different Types of Vesting Data Here
      -------------------------------------------------------------------------

      vestingValue ::Value
      vestingValue = if valueAmount P.> vestingAmount
        then Value.singleton (vcPolicyID vc) (vcTokenName vc) vestingAmount
        else Value.singleton (vcPolicyID vc) (vcTokenName vc) valueAmount

      returnValue ::Value
      returnValue = if valueAmount P.> vestingAmount
        then Value.singleton (vcPolicyID vc) (vcTokenName vc) (valueAmount P.- vestingAmount)
        else Value.singleton (vcPolicyID vc) (vcTokenName vc) (0 :: Integer)

      tokenValue :: Value
      tokenValue = case Contexts.findOwnInput context of
        Nothing     -> traceError "No Input to Validate."
        Just input  -> txOutValue $ txInInfoResolved $ input

      vestingAmount :: Integer
      vestingAmount = cdtVestAmount datum

      -- | The integer amount of the value inside the script UTxO.
      valueAmount :: Integer
      valueAmount = Value.valueOf tokenValue (vcPolicyID vc) (vcTokenName vc)

      vestingPKH :: PubKeyHash
      vestingPKH = cdtVestingUserPKH datum

      vestingAddr :: Address
      vestingAddr = pubKeyHashAddress vestingPKH

      vestingGroup :: [PubKeyHash]
      vestingGroup = cdtVestingGroupPKH datum

      treasuryPKH :: PubKeyHash
      treasuryPKH = cdtTreasuryPKH datum

      treasuryAddr :: Address
      treasuryAddr = pubKeyHashAddress treasuryPKH

      -------------------------------------------------------------------------
      -- | Helper Functions Here
      -------------------------------------------------------------------------

      -- | This may be incorrect. Check plutus repo...
      beginningOfTime :: Integer
      beginningOfTime = 1596059091000
      
      -- | This is the default time. Check Plutus repo...
      def :: Ledger.TimeSlot.SlotConfig
      def = Ledger.TimeSlot.SlotConfig 
        { Ledger.TimeSlot.scSlotLength = 1000
        , Ledger.TimeSlot.scSlotZeroTime = Time.POSIXTime beginningOfTime
        }
      
      -- | Check if a signee has signed the pending transaction.
      checkTxSigner :: PubKeyHash -> Bool
      checkTxSigner signee = Contexts.txSignedBy info signee  -- Not Working as of 1.30.1

      -- | Search each TxOut for the correct address and value.
      checkTxOutForValueAtAddress :: [TxOut] -> Address -> Value -> Bool
      checkTxOutForValueAtAddress [] _addr _val = False
      checkTxOutForValueAtAddress (x:xs) addr val
        | ((txOutAddress x) P.== addr) P.&& ((txOutValue x) P.== val) = True
        | otherwise                                                   = checkTxOutForValueAtAddress xs addr val
      
      -- | Search each TxOut for the correct value.
      checkTxOutForValue :: [TxOut] -> Value -> Bool
      checkTxOutForValue [] _val = False
      checkTxOutForValue (x:xs) val
        | (txOutValue x) P.== val = True
        | otherwise               = checkTxOutForValue xs val

      
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

typedValidator :: VestingContractParams -> Scripts.TypedValidator Typed
typedValidator vc = Scripts.mkTypedValidator @Typed
  ($$(PlutusTx.compile [|| mkValidator ||]) `PlutusTx.applyCode` PlutusTx.liftCode vc)
  $$(PlutusTx.compile  [|| wrap        ||])
    where
      wrap = Scripts.wrapValidator @CustomDatumType @CustomRedeemerType  -- @Datum @Redeemer


-------------------------------------------------------------------------------
-- | Off Chain
-------------------------------------------------------------------------------

data SupplyParams = SupplyParams
    { sAmount   :: !Integer
    , sDeadline :: !Integer
    , sVester   :: !PubKeyHash
    , sGroup    :: ![PubKeyHash]
    } deriving (Generic, ToJSON, FromJSON, ToSchema)

type Schema =
    Endpoint "supply" SupplyParams
    .\/ Endpoint "redeem" ()
    .\/ Endpoint "refund" ()

contract :: AsContractError e => Contract () Schema e ()
contract = selectList [supply, redeem, refund] >> contract

-- | The redeem endpoint.
redeem :: AsContractError e => Promise () Schema e ()
redeem =  endpoint @"redeem" @() $ \() -> do
    buyer          <- pubKeyHash P.<$> Plutus.Contract.ownPubKey
    unspentOutputs <- utxosAt (Ledger.scriptAddress $ Scripts.validatorScript (typedValidator vestingContractParams))
    let tx = collectFromScript unspentOutputs Redeem PlutusTx.Prelude.<> Constraints.mustPayToPubKey buyer (Ada.lovelaceValueOf 1)
    void $ submitTxConstraintsSpending (typedValidator vestingContractParams) unspentOutputs tx

-- | The refund endpoint.
refund :: AsContractError e => Promise () Schema e ()
refund =  endpoint @"refund" @() $ \() -> do
    buyer          <- pubKeyHash P.<$> Plutus.Contract.ownPubKey
    unspentOutputs <- utxosAt (Ledger.scriptAddress $ Scripts.validatorScript (typedValidator vestingContractParams))
    let tx = collectFromScript unspentOutputs Refund PlutusTx.Prelude.<> Constraints.mustPayToPubKey buyer (Ada.lovelaceValueOf 1)
    void $ submitTxConstraintsSpending (typedValidator vestingContractParams) unspentOutputs tx

-- | The supply endpoint.
supply :: AsContractError e => Promise () Schema e ()
supply =  endpoint @"supply" @SupplyParams $ \(SupplyParams {..}) -> do
    treasury <- pubKeyHash P.<$> Plutus.Contract.ownPubKey
    let vestDatum = CustomDatumType { cdtVestAmount      = sAmount
                                    , cdtVestDeadline    = sDeadline
                                    , cdtVestingUserPKH  = sVester
                                    , cdtVestingGroupPKH = sGroup
                                    , cdtTreasuryPKH     = treasury
                                    }
    logInfo @String $ "DATUM HASH"
    logInfo @DatumHash $ Ledger.datumHash (Datum (PlutusTx.toBuiltinData vestDatum))
    let supplyValue = Value.singleton (vcPolicyID vestingContractParams) (vcTokenName vestingContractParams) sAmount
    let tx = Constraints.mustPayToTheScript vestDatum supplyValue PlutusTx.Prelude.<> Constraints.mustBeSignedBy treasury PlutusTx.Prelude.<> Constraints.mustIncludeDatum (Datum $ PlutusTx.toBuiltinData vestDatum  )
    void $ submitTxConstraints (typedValidator vestingContractParams) tx
    logInfo @String $ "Treasury Supplied the Supply."

-------------------------------------------------------------------------------
-- | The code below is required for the PAB to compile.
-------------------------------------------------------------------------------

endpoints :: AsContractError e => Contract () Schema e ()
endpoints = contract

mkSchemaDefinitions ''Schema

$(mkKnownCurrencies [])

-------------------------------------------------------------------------------
-- | The code below is required for the plutus script to compile.
-------------------------------------------------------------------------------

-- script :: Plutus.Script
-- script = Plutus.unValidatorScript validator

-- vestingContractScriptShortBs :: SBS.ShortByteString
-- vestingContractScriptShortBs = SBS.toShort . LBS.toStrict $ serialise script

-- vestingContractScript :: PlutusScript PlutusScriptV1
-- vestingContractScript = PlutusScriptSerialised vestingContractScriptShortBs