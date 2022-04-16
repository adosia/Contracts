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
  , (===)
  , ShippingInfoType
  , siCustomerPKH
  , siOfferPrice
  , siPrinterPKH
  , siShipTime
  , PrinterRegistrationType
  , prPrinterPKH
  , prPrinterInfo
  , prUUIDHash
  , findOtherPKH
  ) where
import           Ledger                    hiding ( singleton )
import           Playground.Contract
import qualified PlutusTx
import           PlutusTx.Prelude
import qualified Plutus.V1.Ledger.Contexts  as Contexts

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

instance Equiv PrintingPoolType PrintingInfoType where
  {-# INLINABLE (===) #-}
  a === b = ( ppCustomerPKH a == piCustomerPKH b) &&
            ( ppOfferPrice  a == piOfferPrice  b)
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

-------------------------------------------------------------------------------
-- | Printer Registration Object
-------------------------------------------------------------------------------
data PrinterRegistrationType = PrinterRegistrationType
  { prPrinterPKH  :: !PubKeyHash
  -- ^ The printer's payment public key hash.
  , prPrinterInfo :: !BuiltinByteString
  -- ^ A brief string of info / hash of info.
  , prUUIDHash    :: !BuiltinByteString
  -- ^ Hash Of UUID required for stake keys.
  }
    deriving stock (Show, Generic)
    deriving anyclass (FromJSON, ToJSON, ToSchema)
PlutusTx.unstableMakeIsData ''PrinterRegistrationType
PlutusTx.makeLift ''PrinterRegistrationType

instance Eq PrinterRegistrationType where
  {-# INLINABLE (==) #-}
  a == b = ( prPrinterPKH  a == prPrinterPKH  b) &&
           ( prPrinterInfo a == prPrinterInfo b) &&
           ( prUUIDHash    a == prUUIDHash    b)

-------------------------------------------------------------------------------
-- | Find the PubKeyHash of the other UTxO in the transaction.
-------------------------------------------------------------------------------
findOtherPKH :: TxInfo -> [TxInInfo]  -> Maybe PubKeyHash
findOtherPKH _ [] = Nothing
findOtherPKH info (x:xs)  = 
  case txOutDatumHash $ txInInfoResolved x of
    Nothing    -> findOtherPKH info xs
    Just datumHash' -> case Contexts.findDatum datumHash' info of
      Nothing             -> findOtherPKH info xs
      Just (Datum datum') -> case PlutusTx.unsafeFromBuiltinData datum' of
        (PrinterRegistrationType pkh' _ _) -> Just pkh'