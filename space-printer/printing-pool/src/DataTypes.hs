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
  ( PrintingPoolType (..)
  , changeRegionCodes
  , OfferInformationType (..)
  , checkPrintingOffer
  , ShippingInfoType (..)
  , checkShippingStatus
  ) where
import qualified PlutusTx
import           PlutusTx.Prelude
import qualified Plutus.V2.Ledger.Api   as PlutusV2
{- |
  Author   : The Ancient Kraken
  Copyright: 2022
  Version  : Rev 1
-}
-------------------------------------------------------------------------------
-- | Printing Pool Data Object
-------------------------------------------------------------------------------
data PrintingPoolType = PrintingPoolType
  { ppCustomerPKH :: PlutusV2.PubKeyHash
  -- ^ The customer's payment public key hash.
  , ppCustomerSC  :: PlutusV2.PubKeyHash
  -- ^ The Customer's staking credential.
  , ppRegionCode  :: [Integer]
  -- ^ The lovelace amount for the printer.
  , ppPOName      :: PlutusV2.TokenName
  -- ^ The Purchase Order Name.
  }
PlutusTx.unstableMakeIsData ''PrintingPoolType

changeRegionCodes :: PrintingPoolType -> PrintingPoolType -> Bool
changeRegionCodes a b = ( ppCustomerPKH a == ppCustomerPKH b ) &&
                        ( ppCustomerSC  a == ppCustomerSC  b ) &&
                        ( ppRegionCode  a /= ppRegionCode  b ) &&
                        ( ppPOName      a == ppPOName      b )

-------------------------------------------------------------------------------
-- | Make Offer Data Object
-------------------------------------------------------------------------------
data OfferInformationType = OfferInformationType
  { oiCustomerPKH :: PlutusV2.PubKeyHash
  -- ^ The customer's payment public key hash.
  , oiCustomerSC  :: PlutusV2.PubKeyHash
  -- ^ The Customer's staking credential.
  , oiRegionCode  :: [Integer]
  -- ^ The lovelace amount for the printer.
  , oiPOName      :: PlutusV2.TokenName
  -- ^ The Purchase Order Name.
  , oiPrinterPKH  :: PlutusV2.PubKeyHash
  -- ^ The printer's payment public key hash.
  , oiPrinterSC   :: PlutusV2.PubKeyHash
  -- ^ The printer's payment public key hash.
  , oiOfferPrice  :: Integer
  -- ^ The lovelace amount for the printer.
  , oiPrintTime   :: Integer
  -- ^ The estimated printing time in nanoseconds.
  }
PlutusTx.unstableMakeIsData ''OfferInformationType

checkPrintingOffer :: PrintingPoolType -> OfferInformationType -> Bool
checkPrintingOffer a b =  ( ppCustomerPKH a == oiCustomerPKH b ) &&
                          ( ppCustomerSC  a == oiCustomerSC  b ) &&
                          ( ppRegionCode  a == oiRegionCode  b ) &&
                          ( ppPOName      a == oiPOName      b )

-------------------------------------------------------------------------------
-- | Shipping Data Object
-------------------------------------------------------------------------------
data ShippingInfoType = ShippingInfoType
  { siCustomerPKH :: PlutusV2.PubKeyHash
  -- ^ The customer's payment public key hash.
  , siCustomerSC  :: PlutusV2.PubKeyHash
  -- ^ The Customer's staking credential.
  , siPrinterPKH  :: PlutusV2.PubKeyHash
  -- ^ The printer's payment public key hash.
  , siPrinterSC   :: PlutusV2.PubKeyHash
  -- ^ The Customer's staking credential.
  , siOfferPrice  :: Integer
  -- ^ The lovelace amount for the printer.
  , siPOName      :: PlutusV2.TokenName
  -- ^ The Purchase Order Name.
  }
PlutusTx.unstableMakeIsData ''ShippingInfoType

checkShippingStatus :: OfferInformationType -> ShippingInfoType -> Bool
checkShippingStatus a b = ( oiCustomerPKH a == siCustomerPKH b ) &&
                          ( oiCustomerSC  a == siCustomerSC  b ) &&
                          ( oiPrinterPKH  a == siPrinterPKH  b ) &&
                          ( oiPrinterSC   a == siPrinterSC   b ) &&
                          ( oiOfferPrice  a == siOfferPrice  b ) &&
                          ( oiPOName      a == siPOName      b )

