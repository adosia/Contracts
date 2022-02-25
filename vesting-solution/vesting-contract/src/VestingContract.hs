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

module VestingContract
  ( vestingContractScript
  , vestingContractScriptShortBs
  , Schema
  , contract
  ) where

import           Cardano.Api.Shelley       (PlutusScript (..), PlutusScriptV1)
import           Control.Monad             (void)

import           Codec.Serialise           ( serialise )

import qualified Data.ByteString.Lazy      as LBS
import qualified Data.ByteString.Short     as SBS
import qualified Data.Maybe

import           Playground.Contract
import           Plutus.Contract
import           Plutus.Contract.Trace as X

import Ledger
import qualified Ledger.Typed.Scripts      as Scripts

import qualified PlutusTx
import           PlutusTx.Prelude      hiding (pure, (<$>))
import qualified Prelude               as Haskell


import qualified Plutus.V1.Ledger.Scripts  as Plutus
import qualified Plutus.V1.Ledger.Ada      as Ada

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


-------------------------------------------------------------------------------
-- | Create the datum parameters data object.
-------------------------------------------------------------------------------

data CustomDatumType = CustomDatumType 
  { cdtVestAmount      :: !Value
  -- ^ The amount of lovelace the user is allowed to extract.
  , cdtVestDeadline    :: !POSIXTime
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

data CustomRedeemerType = CustomRedeemerType
  { crtAction :: !Integer }
    -- deriving stock (Show, Generic)
    -- deriving anyclass (FromJSON, ToJSON, ToSchema)
PlutusTx.unstableMakeIsData ''CustomRedeemerType
PlutusTx.makeLift ''CustomRedeemerType


-------------------------------------------------------------------------------
-- | Define The Token Sale Parameters Here
-------------------------------------------------------------------------------

validator :: Plutus.Validator
validator = Scripts.validatorScript (typedValidator vc)
  where 
    vc = VestingContractParams
      { vcMajorityParam = 3 -- | This may need to be inside the datum
      , vcPolicyID      = "5243f6530c3507a3ed1217848475abb5ec0ec122e00c82e878ff2292"  -- | sample pid
      , vcTokenName     = "TokenC"  -- | sample tn
      }


-------------------------------------------------------------------------------
-- | mkValidator :: Data -> Datum -> Redeemer -> ScriptContext -> Bool
-------------------------------------------------------------------------------
{-# INLINABLE mkValidator #-}
mkValidator :: VestingContractParams -> CustomDatumType -> CustomRedeemerType -> ScriptContext -> Bool
mkValidator vc datum redeemer context 
  | checkRedeemer = True
  | otherwise     = False
    where
      -------------------------------------------------------------------------
      -- | Use the redeemer to switch validators.
      -------------------------------------------------------------------------
      
      checkRedeemer :: Bool
      checkRedeemer
        | action == 0 = True -- retrieve  -- | Retrieve vesting amount
        | action == 1 = True -- remove    -- | Remove user from vesting contract.
        | otherwise     = False
      
      action :: Integer
      action = (crtAction redeemer)

      -------------------------------------------------------------------------
      -- | Different Types of Validators Here
      -------------------------------------------------------------------------
      
      -- | Put all the retrieve functions together here.
      -- retrieve :: Bool
      -- retrieve = do
      --   { let a = checkTxSigner vestingPKH
      --   ; let b = checkTxOutForValueAtAddress currentTxOutputs vestingAddr vestingValue
      --   ; let c = checkTxOutForValue scriptTxOutputs returnValue
      --   -- ; let d = (Ledger.TimeSlot.slotToBeginPOSIXTime def (Slot $ cdtVestDeadline datum)) `before` Contexts.txInfoValidRange info
      --   ; all (==(True :: Bool)) [a,b,c,d]
      --   }
      
      -- -- | Put all the retrieve functions together here.
      -- remove :: Bool
      -- remove = do
      --   { let a = True
      --   ; let b = True
      --   ; all (==(True :: Bool)) [a,b]
      --   }

      -------------------------------------------------------------------------
      -- | Script Info and TxOutputs
      -------------------------------------------------------------------------
      
      -- The script info.
      -- info :: TxInfo
      -- info = scriptContextTxInfo context

      -- -- All the current outputs.
      -- currentTxOutputs :: [TxOut]
      -- currentTxOutputs = txInfoOutputs info

      -- -- All the outputs going back to the script.
      -- scriptTxOutputs  :: [TxOut]
      -- scriptTxOutputs  = Contexts.getContinuingOutputs context

      -------------------------------------------------------------------------
      -- | Different Types of Vesting Data Here
      -------------------------------------------------------------------------

      -- vestingValue ::Value
      -- vestingValue = if valueAmount > vestingAmount
      --   then Value.singleton (vcPolicyID vc) (vcTokenName vc) vestingAmount
      --   else Value.singleton (vcPolicyID vc) (vcTokenName vc) valueAmount

      -- returnValue ::Value
      -- returnValue = if valueAmount > vestingAmount
      --   then Value.singleton (vcPolicyID vc) (vcTokenName vc) (valueAmount - vestingAmount)
      --   else Value.singleton (vcPolicyID vc) (vcTokenName vc) (0 :: Integer)

      -- tokenValue :: Value
      -- tokenValue = case Contexts.findOwnInput context of
      --   Nothing     -> traceError "No Input to Validate."
      --   Just input  -> txOutValue $ txInInfoResolved $ input

      -- vestingAmount :: Integer
      -- vestingAmount = cdtVestAmount datum

      -- | The integer amount of the value inside the script UTxO.
      -- valueAmount :: Integer
      -- valueAmount = Value.valueOf tokenValue (vcPolicyID vc) (vcTokenName vc)

      -- vestingPKH :: PubKeyHash
      -- vestingPKH = cdtVestingUserPKH datum

      -- vestingAddr :: Address
      -- vestingAddr = pubKeyHashAddress vestingPKH

      -- vestingGroup :: [PubKeyHash]
      -- vestingGroup = cdtVestingGroupPKH datum

      -- treasuryPKH :: PubKeyHash
      -- treasuryPKH = cdtTreasuryPKH datum

      -- treasuryAddr :: Address
      -- treasuryAddr = pubKeyHashAddress treasuryPKH

      -------------------------------------------------------------------------
      -- | Helper Functions Here
      -------------------------------------------------------------------------

      -- | This may be incorrect. Check plutus repo...
      -- beginningOfTime :: Integer
      -- beginningOfTime = 1596059091000
      
      -- | This is the default time. Check Plutus repo...
      -- def :: Ledger.TimeSlot.SlotConfig
      -- def = Ledger.TimeSlot.SlotConfig 
      --   { Ledger.TimeSlot.scSlotLength = 1000
      --   , Ledger.TimeSlot.scSlotZeroTime = Time.POSIXTime beginningOfTime
      --   }
      
      -- | Check if a signee has signed the pending transaction.
      -- checkTxSigner :: PubKeyHash -> Bool
      -- checkTxSigner signee = txSignedBy info signee  -- Not Working as of 1.30.1

      -- -- | Search each TxOut for the correct address and value.
      -- checkTxOutForValueAtAddress :: [TxOut] -> Address -> Value -> Bool
      -- checkTxOutForValueAtAddress [] _addr _val = False
      -- checkTxOutForValueAtAddress (x:xs) addr val
      --   | ((txOutAddress x) == addr) && ((txOutValue x) == val) = True
      --   | otherwise                                                   = checkTxOutForValueAtAddress xs addr val
      
      -- -- | Search each TxOut for the correct value.
      -- checkTxOutForValue :: [TxOut] -> Value -> Bool
      -- checkTxOutForValue [] _val = False
      -- checkTxOutForValue (x:xs) val
      --   | (txOutValue x) == val = True
      --   | otherwise               = checkTxOutForValue xs val

      
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
-- | The code below is required for the plutus script compile.
-------------------------------------------------------------------------------

script :: Plutus.Script
script = Plutus.unValidatorScript validator

vestingContractScriptShortBs :: SBS.ShortByteString
vestingContractScriptShortBs = SBS.toShort . LBS.toStrict $ serialise script

vestingContractScript :: PlutusScript PlutusScriptV1
vestingContractScript = PlutusScriptSerialised vestingContractScriptShortBs


-------------------------------------------------------------------------------
-- | Off Chain
-------------------------------------------------------------------------------
type Schema =
  Endpoint "" ()

contract :: AsContractError e => Contract () Schema e ()
contract = selectList [] >> contract