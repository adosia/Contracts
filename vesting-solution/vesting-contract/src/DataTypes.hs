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
module DataTypes
  ( CustomDatumType
  , cdtRewardParams
  , cdtDeadlineParams
  , cdtVestingStage
  , cdtVestingUserPKH
  , cdtTreasuryPKH
  , cdtVotingGroupPKHs
  , cdtVotingWeights
  ) where
import           Playground.Contract
import           Ledger
import qualified PlutusTx
import           PlutusTx.Prelude
-------------------------------------------------------------------------------
-- | Create the datum parameters data object.
-------------------------------------------------------------------------------
data CustomDatumType = CustomDatumType
  { cdtVestingStage    :: !Integer
  -- ^ The stage determines the deadline and reward.
  , cdtVestingUserPKH  :: !PubKeyHash
  -- ^ The public key hash of the receiver.
  , cdtVotingGroupPKHs :: ![PubKeyHash]
  -- ^ A list public key hashes of everyone who is voting with the contract.
  , cdtVotingWeights   :: ![Integer]
  -- ^ A list voting weights of everyone who is vesting with the contract.
  , cdtTreasuryPKH     :: !PubKeyHash
  -- ^ The public key hash of the treasury wallet.
  , cdtDeadlineParams  :: ![Integer]
  -- ^ The deadline function parameters [deltaT, t0]
  , cdtRewardParams    :: ![Integer]
  -- ^ The reward   function parameters [deltaV, v0]
  }
    deriving stock    (Show, Generic)
    deriving anyclass (FromJSON, ToJSON, ToSchema)
PlutusTx.unstableMakeIsData ''CustomDatumType
PlutusTx.makeLift ''CustomDatumType
-- old is a; new is b
instance Eq CustomDatumType where
  {-# INLINABLE (==) #-}
  a == b = ( cdtVestingStage a + 1 == cdtVestingStage    b) &&
           ( cdtVestingUserPKH   a == cdtVestingUserPKH  b) &&
           ( cdtVotingGroupPKHs  a == cdtVotingGroupPKHs b) &&
           ( cdtVotingWeights    a == cdtVotingWeights   b) &&
           ( cdtTreasuryPKH      a == cdtTreasuryPKH     b) &&
           ( cdtRewardParams     a == cdtRewardParams    b) &&
           ( head (cdtDeadlineParams a) == head (cdtDeadlineParams b)) &&
           ( head (tail (cdtDeadlineParams a)) + head (cdtDeadlineParams a) == head (tail (cdtDeadlineParams b)))