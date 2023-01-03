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
  ( MarketDataType (..)
  , IncreaseData (..)
  , NewDesignerData (..)
  , checkDatumIncrease
  , updateSalePrice
  , checkNewDatum
  ) where
import qualified PlutusTx
import           PlutusTx.Prelude
import qualified Plutus.V2.Ledger.Api as PlutusV2
{- |
  Author   : The Ancient Kraken
  Copyright: 2022
-}
-------------------------------------------------------------------------------
-- | Custom Data Object
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

updateSalePrice :: MarketDataType -> MarketDataType -> Bool
updateSalePrice a b = ( mDesignerPKH a == mDesignerPKH b ) &&
                      ( mDesignerSC  a == mDesignerSC  b ) &&
                      ( mStartName   a == mStartName   b ) &&
                      ( mNumber      a == mNumber      b ) &&
                      ( mPoPolicy    a == mPoPolicy    b )
-------------------------------------------------------------------------------
-- | Update Data Structure
-------------------------------------------------------------------------------
data IncreaseData = IncreaseData
  { uInc :: Integer
  -- ^ The potential lovelace increase required for an update.
  }
PlutusTx.unstableMakeIsData ''IncreaseData

-------------------------------------------------------------------------------
-- | New Owner Data Structure
-------------------------------------------------------------------------------
data NewDesignerData = NewDesignerData
  { newDesignerPKH :: PlutusV2.PubKeyHash
  -- ^ The new Designer's payment public key hash.
  , newDesignerSC  :: PlutusV2.PubKeyHash
  -- ^ The new Designer's staking credential.
  }
PlutusTx.unstableMakeIsData ''NewDesignerData

-- old datum, redeemer, new datum
checkNewDatum :: MarketDataType -> NewDesignerData -> MarketDataType -> Bool
checkNewDatum a b c = ( newDesignerPKH b == mDesignerPKH c ) &&
                      ( newDesignerSC  b == mDesignerSC  c ) &&
                      ( mStartName     a == mStartName   c ) &&
                      ( mNumber        a == mNumber      c ) &&
                      ( mPoPolicy      a == mPoPolicy    c ) &&
                      ( mPoPrice       a == mPoPrice     c ) &&
                      ( mPoFreeFlag    a == mPoFreeFlag  c )