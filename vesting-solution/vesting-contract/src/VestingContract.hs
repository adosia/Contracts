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
  , CustomDatumType
  , lockInterval
  , rewardFunction
  ) where

import           Cardano.Api.Shelley       (PlutusScript (..), PlutusScriptV1)

import           Codec.Serialise           ( serialise )

import qualified Data.ByteString.Lazy      as LBS
import qualified Data.ByteString.Short     as SBS

import           Playground.Contract
import           Plutus.Contract

import           Ledger
import qualified Ledger.Typed.Scripts      as Scripts

import qualified PlutusTx
import           PlutusTx.Prelude

import qualified Plutus.V1.Ledger.Scripts  as Plutus
import qualified Plutus.V1.Ledger.Interval as Interval
import qualified Plutus.V1.Ledger.Time     as Time


{- |
  Author   : The Ancient Kraken
  Copyright: 2021
  Version  : Rev 0

  This is a vesting contract.
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
  , vcMasterKey     :: !PubKeyHash
  -- ^ Experimental master key by pass
  }
PlutusTx.makeLift ''VestingContractParams


-------------------------------------------------------------------------------
-- | Create the datum parameters data object.
-------------------------------------------------------------------------------

data CustomDatumType = CustomDatumType
  { cdtVestingStage    :: !Integer
  -- ^ The stage determines the deadline and reward.
  , cdtVestingUserPKH  :: !PubKeyHash
  -- ^ The public key hash of the receiver.
  , cdtVestingGroupPKH :: ![PubKeyHash]
  -- ^ A list public key hashes of everyone who is vesting with the contract.
  , cdtTreasuryPKH     :: !PubKeyHash
  -- ^ The public key hash of the treasury wallet.
  , cdtDeadlineParams  :: ![Integer]
  -- ^ The deadline function parameters [deltaT, t0]
  , cdtRewardParams    :: ![Integer]
  -- ^ The reward function parameters [deltaV, v0]
  }
PlutusTx.unstableMakeIsData ''CustomDatumType
PlutusTx.makeLift ''CustomDatumType

instance Eq CustomDatumType where
  {-# INLINABLE (==) #-}
  a == b = ( cdtVestingStage    a == cdtVestingStage b + 1) &&
           ( cdtVestingUserPKH  a == cdtVestingUserPKH   b) &&
           ( cdtVestingGroupPKH a == cdtVestingGroupPKH  b) &&
           ( cdtTreasuryPKH     a == cdtTreasuryPKH      b) &&
           ( cdtDeadlineParams  a == cdtDeadlineParams   b) &&
           ( cdtRewardParams    a == cdtRewardParams     b)


-------------------------------------------------------------------------------
-- | Create the redeemer parameters data object.
-------------------------------------------------------------------------------

newtype CustomRedeemerType = CustomRedeemerType
  { crtAction :: Integer }
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
      { vcMajorityParam = 3
      , vcPolicyID      = "5243f6530c3507a3ed1217848475abb5ec0ec122e00c82e878ff2292"
      , vcTokenName     = "TokenC"
      , vcMasterKey     = ""
      }

-------------------------------------------------------------------------------
-- | Deadline and Reward Functions
-------------------------------------------------------------------------------

-- Pick the locking interval
lockInterval :: CustomDatumType -> Interval POSIXTime
lockInterval datum' = Interval.interval (integerToPOSIX startingTime) (integerToPOSIX endingTime)
  where
    refEpoch :: Integer
    -- refEpoch = 312 -- mainnet
    refEpoch = 178 -- testnet

    timeTilRefEpoch :: Integer
    -- timeTilRefEpoch = 1640987100000  -- mainnet
    timeTilRefEpoch = 1640895900000  -- testnet

    lengthOfDay :: Integer
    lengthOfDay = 1000*60*60*24

    lengthOfEpoch :: Integer
    lengthOfEpoch = 5 * lengthOfDay

    -- pick some epoch to start
    startEpoch :: Integer
    startEpoch = head $ tail $ cdtDeadlineParams datum'

    -- pick some number of days for the vesting period
    lockedPeriod :: Integer
    lockedPeriod = head $ cdtDeadlineParams datum'

    startingTime :: Integer
    startingTime = timeTilRefEpoch + (startEpoch - refEpoch)*lengthOfEpoch

    endingTime :: Integer
    endingTime = startingTime + lockedPeriod*lengthOfDay

    -- Number of milliseconds from unix time start
    integerToPOSIX :: Integer -> POSIXTime
    integerToPOSIX x = Time.fromMilliSeconds $ Time.DiffMilliSeconds x

-- Assume Linear reward
rewardFunction :: CustomDatumType -> Integer -> Integer
rewardFunction datum' t = v0 - t * deltaV
  where
    -- starting amount
    v0 :: Integer
    v0 = head $ tail $ cdtRewardParams datum'

    -- amount reduced every period
    deltaV :: Integer
    deltaV = head $ cdtRewardParams datum'

-------------------------------------------------------------------------------
-- | mkValidator :: Data -> Datum -> Redeemer -> ScriptContext -> Bool
-------------------------------------------------------------------------------
{-# INLINABLE mkValidator #-}
mkValidator :: VestingContractParams -> CustomDatumType -> CustomRedeemerType -> ScriptContext -> Bool
mkValidator _ _ redeemer _
  | checkRedeemer = True
  | otherwise     = False
    where
      -------------------------------------------------------------------------
      -- | Use the redeemer to switch validators.
      -------------------------------------------------------------------------
      checkRedeemer :: Bool
      checkRedeemer
        | action == 0 = True
        | otherwise   = False

      action :: Integer
      action = crtAction redeemer


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