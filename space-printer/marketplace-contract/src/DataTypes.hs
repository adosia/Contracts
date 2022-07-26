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
  ( MarketDataType
  , mDesignerPKH
  , mDesignerSC
  , mStartPolicy
  , mStartName
  , mNumber
  , mPoPolicy
  , mPrefixName
  , mPoPrice
  ) where
import           PlutusTx.Prelude
import qualified PlutusTx
import           Ledger                   hiding ( singleton )
import           Data.Aeson               ( FromJSON, ToJSON )
import           Data.OpenApi.Schema      ( ToSchema )
import           GHC.Generics             ( Generic )
import           Prelude                  ( Show )
{- |
  Author   : The Ancient Kraken
  Copyright: 2022
  Version  : Rev 0
-}
-------------------------------------------------------------------------------
-- | Custom Data Object
-------------------------------------------------------------------------------
data MarketDataType = MarketDataType
  { mDesignerPKH :: !PubKeyHash
  -- ^ The Designer's payment public key hash.
  , mDesignerSC  :: !PubKeyHash
  -- ^ The Designer's staking credential.
  , mStartPolicy :: !CurrencySymbol
  -- ^ Starting Policy ID for the designer.
  , mStartName   :: !TokenName
  -- ^ Starting Token Name for the designer.
  , mNumber      :: !Integer
  -- ^ The current design increment number.
  , mPoPolicy    :: !CurrencySymbol
  -- ^ The purchase order Policy ID.
  , mPrefixName  :: !BuiltinByteString
  -- ^ The purchase order Token Name Prefix.
  , mPoPrice     :: !Integer
  -- ^ The purchase order price in lovelace.
  }
    deriving stock    ( Show, Generic )
    deriving anyclass ( FromJSON, ToJSON, ToSchema )
PlutusTx.unstableMakeIsData ''MarketDataType
PlutusTx.makeLift ''MarketDataType

-- old == new
instance Eq MarketDataType where
  {-# INLINABLE (==) #-}
  a == b = ( mDesignerPKH a == mDesignerPKH b ) &&
           ( mDesignerSC  a == mDesignerSC  b ) &&
           ( mStartPolicy a == mStartPolicy b ) &&
           ( mStartName   a == mStartName   b ) &&
           ( mNumber  a + 1 == mNumber      b ) &&
           ( mPoPolicy    a == mPoPolicy    b ) &&
           ( mPoPrice     a == mPoPrice     b )
