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
  , ppOfferPrice
  , PrintingInfoType
  , piCustomerPKH
  , piOfferPrice
  , piPrinterPKH
  , piPrintTime
  , OfferInformationType
  , oiCustomerPKH
  , oiOfferPrice
  , oiPrinterPKH
  , oiPrintTime
  , ShippingInfoType
  , siCustomerPKH
  , siOfferPrice
  , siPrinterPKH
  , siShipTime
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
  , ppOfferPrice  :: !Integer
  -- ^ The lovelace amount for the printer.
  }
    deriving stock (Show, Generic)
    deriving anyclass (FromJSON, ToJSON, ToSchema)
PlutusTx.unstableMakeIsData ''PrintingPoolType
PlutusTx.makeLift ''PrintingPoolType

instance Eq PrintingPoolType where
  {-# INLINABLE (==) #-}
  a == b = ( ppCustomerPKH a == ppCustomerPKH b) &&
           ( ppOfferPrice  a /= ppOfferPrice  b)
-------------------------------------------------------------------------------
-- | Make Offer Data Object
-------------------------------------------------------------------------------
data OfferInformationType = OfferInformationType
  { oiCustomerPKH :: !PubKeyHash
  -- ^ The customer's payment public key hash.
  , oiOfferPrice  :: !Integer
  -- ^ The lovelace amount for the printer.
  , oiPrinterPKH  :: !PubKeyHash
  -- ^ The printer's payment public key hash.
  , oiPrintTime   :: !Integer
  -- ^ The estimated printing time in nanoseconds.
  }
    deriving stock (Show, Generic)
    deriving anyclass (FromJSON, ToJSON, ToSchema)
PlutusTx.unstableMakeIsData ''OfferInformationType
PlutusTx.makeLift ''OfferInformationType

instance Eq OfferInformationType where
  {-# INLINABLE (==) #-}
  a == b = ( oiCustomerPKH a == oiCustomerPKH b) &&
           ( oiOfferPrice  a == oiOfferPrice  b) &&
           ( oiPrinterPKH  a == oiPrinterPKH  b) &&
           ( oiPrintTime   a /= oiPrintTime   b)

instance Equiv PrintingPoolType OfferInformationType where
  {-# INLINABLE (===) #-}
  a === b = ( ppCustomerPKH a == oiCustomerPKH b) &&
            ( ppOfferPrice  a == oiOfferPrice  b)
-------------------------------------------------------------------------------
-- | Printing Info Data Object
-------------------------------------------------------------------------------
data PrintingInfoType = PrintingInfoType
  { piCustomerPKH :: !PubKeyHash
  -- ^ The customer's payment public key hash.
  , piOfferPrice  :: !Integer
  -- ^ The lovelace amount for the printer.
  , piPrinterPKH  :: !PubKeyHash
  -- ^ The printer's payment public key hash.
  , piPrintTime   :: !Integer
  -- ^ The estimated printing time in nanoseconds.
  }
    deriving stock (Show, Generic)
    deriving anyclass (FromJSON, ToJSON, ToSchema)
PlutusTx.unstableMakeIsData ''PrintingInfoType
PlutusTx.makeLift ''PrintingInfoType

instance Eq PrintingInfoType where
  {-# INLINABLE (==) #-}
  a == b = ( piCustomerPKH a == piCustomerPKH b) &&
           ( piOfferPrice  a == piOfferPrice  b) &&
           ( piPrinterPKH  a == piPrinterPKH  b) &&
           ( piPrintTime   a == piPrintTime   b)

instance Equiv PrintingInfoType PrintingPoolType where
  {-# INLINABLE (===) #-}
  a === b = ( piCustomerPKH a == ppCustomerPKH b) &&
            ( piOfferPrice  a == ppOfferPrice  b)

instance Equiv OfferInformationType PrintingInfoType where
  {-# INLINABLE (===) #-}
  a === b = ( oiCustomerPKH a == piCustomerPKH b) &&
            ( oiOfferPrice  a == piOfferPrice  b) &&
            ( oiPrinterPKH  a == piPrinterPKH  b) &&
            ( oiPrintTime   a == piPrintTime   b)
-------------------------------------------------------------------------------
-- | Shipping Data Object
-------------------------------------------------------------------------------
data ShippingInfoType = ShippingInfoType
  { siCustomerPKH :: !PubKeyHash
  -- ^ The customer's payment public key hash.
  , siOfferPrice  :: !Integer
  -- ^ The lovelace amount for the printer.
  , siPrinterPKH  :: !PubKeyHash
  -- ^ The printer's payment public key hash.
  , siShipTime    :: !Integer
  -- ^ The estimated printing time in nanoseconds.
  }
    deriving stock (Show, Generic)
    deriving anyclass (FromJSON, ToJSON, ToSchema)
PlutusTx.unstableMakeIsData ''ShippingInfoType
PlutusTx.makeLift ''ShippingInfoType

instance Eq ShippingInfoType where
  {-# INLINABLE (==) #-}
  a == b = ( siCustomerPKH a == siCustomerPKH b) &&
           ( siOfferPrice  a == siOfferPrice  b) &&
           ( siPrinterPKH  a == siPrinterPKH  b) &&
           ( siShipTime    a == siShipTime    b)

instance Equiv PrintingInfoType ShippingInfoType where
  {-# INLINABLE (===) #-}
  a === b = ( piCustomerPKH a == siCustomerPKH b) &&
            ( piOfferPrice  a == siOfferPrice  b) &&
            ( piPrinterPKH  a == siPrinterPKH  b) &&
            ( siShipTime    b  > (0 :: Integer) )