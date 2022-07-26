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
  ( PrintingPoolType
  , ppCustomerPKH
  , ppCustomerSC
  , ppRegionCode
  , OfferInformationType
  , oiCustomerPKH
  , oiOfferPrice
  , oiPrinterPKH
  , oiPrintTime
  , ShippingInfoType
  , siCustomerPKH
  , siCustomerSC
  , siPrinterPKH
  , siPrinterSC
  , siOfferPrice
  , (===)
  ) where
import           Ledger                    hiding ( singleton )
import           Playground.Contract
import qualified PlutusTx
import           PlutusTx.Prelude
{- |
  Author   : The Ancient Kraken
  Copyright: 2021
  Version  : Rev 0

  cardano-cli 1.33.0 - linux-x86_64 - ghc-8.10
  git rev 814df2c146f5d56f8c35a681fe75e85b905aed5d

  The printing pool smart contract.
-}
class Equiv a b where
  (===) :: a -> b -> Bool

-------------------------------------------------------------------------------
-- | Printing Pool Data Object
-------------------------------------------------------------------------------
data PrintingPoolType = PrintingPoolType
  { ppCustomerPKH :: !PubKeyHash
  -- ^ The customer's payment public key hash.
  , ppCustomerSC  :: !PubKeyHash
  -- ^ The Customer's staking credential.
  , ppRegionCode  :: ![Integer]
  -- ^ The lovelace amount for the printer.
  }
    deriving stock (Show, Generic)
    deriving anyclass (FromJSON, ToJSON, ToSchema)
PlutusTx.unstableMakeIsData ''PrintingPoolType
PlutusTx.makeLift ''PrintingPoolType

-- old == new
instance Eq PrintingPoolType where
  {-# INLINABLE (==) #-}
  a == b = ( ppCustomerPKH a == ppCustomerPKH b) &&
           ( ppCustomerSC  a == ppCustomerSC  b) &&
           ( ppRegionCode  a /= ppRegionCode  b)

-------------------------------------------------------------------------------
-- | Make Offer Data Object
-------------------------------------------------------------------------------
data OfferInformationType = OfferInformationType
  { oiCustomerPKH :: !PubKeyHash
  -- ^ The customer's payment public key hash.
  , oiCustomerSC  :: !PubKeyHash
  -- ^ The Customer's staking credential.
  , oiRegionCode  :: ![Integer]
  -- ^ The lovelace amount for the printer.
  , oiPrinterPKH  :: !PubKeyHash
  -- ^ The printer's payment public key hash.
  , oiPrinterSC   :: !PubKeyHash
  -- ^ The printer's payment public key hash.
  , oiOfferPrice  :: !Integer
  -- ^ The lovelace amount for the printer.
  , oiPrintTime   :: !Integer
  -- ^ The estimated printing time in nanoseconds.
  }
    deriving stock (Show, Generic)
    deriving anyclass (FromJSON, ToJSON, ToSchema)
PlutusTx.unstableMakeIsData ''OfferInformationType
PlutusTx.makeLift ''OfferInformationType

-- multi sig offer equiv instance
instance Equiv PrintingPoolType OfferInformationType where
  {-# INLINABLE (===) #-}
  a === b = ( ppCustomerPKH a == oiCustomerPKH b) &&
            ( ppCustomerSC  a == oiCustomerSC  b) &&
            ( ppRegionCode  a == oiRegionCode  b)

-------------------------------------------------------------------------------
-- | Shipping Data Object
-------------------------------------------------------------------------------
data ShippingInfoType = ShippingInfoType
  { siCustomerPKH :: !PubKeyHash
  -- ^ The customer's payment public key hash.
  , siCustomerSC  :: !PubKeyHash
  -- ^ The Customer's staking credential.
  , siPrinterPKH  :: !PubKeyHash
  -- ^ The printer's payment public key hash.
  , siPrinterSC   :: !PubKeyHash
  -- ^ The Customer's staking credential.
  , siOfferPrice  :: !Integer
  -- ^ The lovelace amount for the printer.
  }
    deriving stock (Show, Generic)
    deriving anyclass (FromJSON, ToJSON, ToSchema)
PlutusTx.unstableMakeIsData ''ShippingInfoType
PlutusTx.makeLift ''ShippingInfoType

-- print to ship equiv
instance Equiv OfferInformationType ShippingInfoType where
  {-# INLINABLE (===) #-}
  a === b = ( oiCustomerPKH a == siCustomerPKH b) &&
            ( oiCustomerSC  a == siCustomerSC  b) &&
            ( oiPrinterPKH  a == siPrinterPKH  b) &&
            ( oiPrinterSC   a == siPrinterSC   b) &&
            ( oiOfferPrice  a == siOfferPrice  b)
