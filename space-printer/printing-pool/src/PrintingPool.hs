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

module PrintingPool
  ( printingPoolScript
  , printingPoolScriptShortBs
  , Schema
  , contract
  , CustomDatumType(..)
  ) where

import           Cardano.Api.Shelley       (PlutusScript (..), PlutusScriptV1)

import           Codec.Serialise           ( serialise )

import qualified Data.ByteString.Lazy      as LBS
import qualified Data.ByteString.Short     as SBS


import           Ledger                    hiding ( singleton )
import qualified Ledger.Typed.Scripts      as Scripts
import           Playground.Contract


import qualified PlutusTx
import           PlutusTx.Prelude
import           Plutus.Contract
import qualified Plutus.V1.Ledger.Ada      as Ada
import qualified Plutus.V1.Ledger.Scripts  as Plutus
import           Data.Maybe

{- |
  Author   : The Ancient Kraken
  Copyright: 2021
  Version  : Rev 0

  cardano-cli 1.33.0 - linux-x86_64 - ghc-8.10
  git rev 814df2c146f5d56f8c35a681fe75e85b905aed5d

  The printing pool smart contract.
-}

-------------------------------------------------------------------------------
-- | Create the contract parameters data object.
-------------------------------------------------------------------------------
data ContractParams = ContractParams {}
PlutusTx.makeLift ''ContractParams


-------------------------------------------------------------------------------
-- | Create the datum parameters data object.
-------------------------------------------------------------------------------
data CustomDatumType = CustomDatumType
  { cdtPrice       :: !Integer
  -- ^ The amount of lovelace the Seller will receive.
  , cdtCustomerPKH :: !PubKeyHash
  -- ^ The Seller's public key hash.
  , cdtPrinterPKH  :: !PubKeyHash
  -- ^ The Printer's public key hash.
  , cdtStateFlag   :: !Integer
  -- A state flag
  , cdtPrintTime   :: !Integer
  -- How long in days
  }
    deriving stock (Show, Generic)
    deriving anyclass (FromJSON, ToJSON, ToSchema)
PlutusTx.unstableMakeIsData ''CustomDatumType
PlutusTx.makeLift ''CustomDatumType

-------------------------------------------------------------------------------
-- | Create the redeemer parameters data object.
-------------------------------------------------------------------------------

data CustomRedeemerType = CustomRedeemerType
  { crtAction     :: !Integer
  -- ^ The action determines which type of validation to use in the contract.
  , crtPrinterPKH :: !PubKeyHash
  -- ^ The potential printer node for a job.
  }
    deriving stock (Show, Generic)
    deriving anyclass (FromJSON, ToJSON, ToSchema)
PlutusTx.unstableMakeIsData ''CustomRedeemerType
PlutusTx.makeLift ''CustomRedeemerType


-------------------------------------------------------------------------------
-- | Define The Token Sale Parameters Here
-------------------------------------------------------------------------------

validator :: Plutus.Validator
validator = Scripts.validatorScript (typedValidator $ ContractParams {})


-------------------------------------------------------------------------------
-- | mkValidator :: Data -> Datum -> Redeemer -> ScriptContext -> Bool
-- | Create a custom validator inside mkValidator.
-------------------------------------------------------------------------------
{-# INLINABLE mkValidator #-}
mkValidator :: ContractParams -> CustomDatumType -> CustomRedeemerType -> ScriptContext -> Bool
mkValidator _ datum redeemer context
  | checkActionFlag = True
  | otherwise       = False
    where
      -------------------------------------------------------------------------
      -- | Define States Here.
      -------------------------------------------------------------------------

      aCustomerHasAPrintingJob :: Integer
      aCustomerHasAPrintingJob = 0

      aPrinterIsMakingAnOffer :: Integer
      aPrinterIsMakingAnOffer = 1

      aJobIsCurrentlyPrinting :: Integer
      aJobIsCurrentlyPrinting = 2

      aJobIsCurrentlyShipping :: Integer
      aJobIsCurrentlyShipping = 3

      -------------------------------------------------------------------------
      -- | Define Contract Flags Here.
      -------------------------------------------------------------------------
      checkActionFlag :: Bool
      checkActionFlag
        | actionFlag == 0 = traceIfFalse "Customer Job Removal Failed"  customerRemovesJob
        | actionFlag == 1 = traceIfFalse "Customer Job Update Failed"   customerUpdatesJob
        | actionFlag == 2 = traceIfFalse "Printer Job Offer Failed"     printerMakesJobOffer
        | actionFlag == 3 = traceIfFalse "Offer Acceptance Failed"      customerAcceptsOffer
        | actionFlag == 4 = traceIfFalse "Offer Denial Failed"          customerDeniesOffer
        | actionFlag == 5 = traceIfFalse "Printer Cancelling Failed"    printerCancelsJob
        | actionFlag == 6 = traceIfFalse "printer Finishing Failed"     printerFinishesJob
        | actionFlag == 7 = traceIfFalse "Customer Confirmation Failed" customerConfirmsJob
        | otherwise       = traceIfFalse "Incorrect Action Flag"        True                 -- This can be used as a bypass
          where
            actionFlag :: Integer
            actionFlag = crtAction redeemer

      -------------------------------------------------------------------------
      -- | Different Types of Validators Here
      -------------------------------------------------------------------------
      -- | A customer removes a job from the job pool
      customerRemovesJob :: Bool
      customerRemovesJob = do
        { let a = traceIfFalse "Incorrect Token Return" $ checkTxOutForValueAtPKH currentTxOutputs customerPKH tokenValue -- token must go back to customer
        ; let b = traceIfFalse "Incorrect Signer"       $ checkTxSigner customerPKH                                       -- customer must sign it
        ; let c = traceIfFalse "Incorrect State"        $ cdtStateFlag datum == aCustomerHasAPrintingJob                  -- token is looking to be printed
        ; let d = traceIfFalse "Too Many Script Inputs"   checkForSingleScriptInput                                       -- single script input
        ; all (==(True :: Bool)) [a,b,c,d]
        }

      -- | A customer can change the price of a printing job inside the pool.
      customerUpdatesJob :: Bool
      customerUpdatesJob = do
        { let cdt = embeddedDatum scriptTxOutputs
        ; let a = traceIfFalse "Incorrect State"      $ cdtStateFlag datum == aCustomerHasAPrintingJob    -- token is looking to be printed
        ; let b = traceIfFalse "Incorrect Signer"     $ checkTxSigner customerPKH                         -- the customer signs it
        ; let c = traceIfFalse "Incorrect Token Cont" $ checkContTxOutForValue scriptTxOutputs tokenValue -- it must go back to the script
        ; let d = traceIfFalse "Incorrect New State"  $ cdtStateFlag   cdt == aCustomerHasAPrintingJob    -- the token must remain looking to be printed
        ; let e = traceIfFalse "Incorrect Customer"   $ cdtCustomerPKH cdt == customerPKH                 -- The customer can not change
        ; let f = traceIfFalse "Incorrect Price"      $ cdtPrice       cdt >= 3000000                     -- minimum price is 3 ada, change later
        ; let g = traceIfFalse "Too Many Script Inputs" checkForSingleScriptInput                         -- single script input
        ; all (==(True :: Bool)) [a,b,c,d,e,f,g]
        }


      -- -- | A printer selects a job from the job pool.
      -- -- This needs some zk thing to prove to the contract they are a printer.
      printerMakesJobOffer :: Bool
      printerMakesJobOffer = do
        { let cdt = embeddedDatum scriptTxOutputs
        ; let a = traceIfFalse "Incorrect Token Cont" $ checkContTxOutForValue scriptTxOutputs tokenValue -- token must go back to script
        ; let b = traceIfFalse ""                     $ True                                              -- Proof that this is a real printer.
        ; let c = traceIfFalse "Incorrect State"      $ cdtStateFlag   datum == aCustomerHasAPrintingJob  -- the token is looking to be printed
        ; let d = traceIfFalse "Incorrect New State"  $ cdtStateFlag   cdt   == aPrinterIsMakingAnOffer   -- the token is getting an offer
        ; let e = traceIfFalse "Incorrect Customer"   $ cdtCustomerPKH cdt   == customerPKH               -- the customer must remain the same
        ; let f = traceIfFalse "Incorrect Price"      $ cdtPrice       cdt   == cdtPrice datum            -- the price must remain constant
        ; let g = traceIfFalse "Incorrect Printer"    $ cdtPrinterPKH  cdt   == potentialPrinterPKH       -- the price must remain constant
        ; let h = traceIfFalse "Incorrect Print Time" $ cdtPrintTime   cdt    > 0                         -- the print time must be greater than some constant
        ; let i = traceIfFalse "Incorrect Signer"     $ checkTxSigner potentialPrinterPKH                 -- The printer must sign it
        ; let j = traceIfFalse "Too Many Script Inputs" checkForSingleScriptInput                         -- single script input
        ; all (==(True :: Bool)) [a,b,c,d,e,f,g,h,i,j]
        }

      -- -- | A printer selects a job from the job pool.
      -- -- This needs some zk thing to prove to the contract they are a printer.
      customerAcceptsOffer :: Bool
      customerAcceptsOffer = do
        { let cdt = embeddedDatum scriptTxOutputs
        ; let a = traceIfFalse "Incorrect Token Cont" $ checkContTxOutForValue scriptTxOutputs tokenValue -- token must go back to script
        ; let b = traceIfFalse "Incorrect State"      $ cdtStateFlag   datum == aPrinterIsMakingAnOffer   -- the token is getting an offer
        ; let c = traceIfFalse "Incorrect New State"  $ cdtStateFlag   cdt   == aJobIsCurrentlyPrinting   -- the token is accepting the offer
        ; let d = traceIfFalse "Incorrect Customer"   $ cdtCustomerPKH cdt   == customerPKH               -- the customer must remain the same
        ; let e = traceIfFalse "Incorrect Price"      $ cdtPrice       cdt   == cdtPrice datum            -- the price must remain constant
        ; let f = traceIfFalse "Incorrect Printer"    $ cdtPrinterPKH  cdt   == confirmedPrinterPKH       -- the price must remain constant
        ; let g = traceIfFalse "Incorrect Print Time" $ cdtPrintTime   cdt   == cdtPrintTime datum        -- the print time must be greater than some constant
        ; let h = traceIfFalse "Incorrect Signer"     $ checkTxSigner customerPKH                         -- customer must sign it
        ; let i = traceIfFalse "Too Many Script Inputs" checkForSingleScriptInput                         -- single script input
        ; all (==(True :: Bool)) [a,b,c,d,e,f,g,h,i]
        }

       -- -- | A printer selects a job from the job pool.
      -- -- This needs some zk thing to prove to the contract they are a printer.
      customerDeniesOffer :: Bool
      customerDeniesOffer = do
        { let cdt = embeddedDatum scriptTxOutputs
        ; let a = traceIfFalse "Incorrect Token Cont" $ checkContTxOutForValue scriptTxOutputs tokenValue -- token must go back to script
        ; let b = traceIfFalse "Incorrect State"      $ cdtStateFlag   datum == aPrinterIsMakingAnOffer   -- the token is getting an offer
        ; let c = traceIfFalse "Incorrect New State"  $ cdtStateFlag   cdt   == aCustomerHasAPrintingJob  -- the token is denying an offer
        ; let d = traceIfFalse "Incorrect Customer"   $ cdtCustomerPKH cdt   == customerPKH               -- the customer must remain the same
        ; let e = traceIfFalse "Incorrect Price"      $ cdtPrice       cdt   == cdtPrice datum            -- the price must remain constant
        ; let f = traceIfFalse "Incorrect Signer"     $ checkTxSigner customerPKH                         -- customer must sign it
        ; let g = traceIfFalse "Too Many Script Inputs" checkForSingleScriptInput                         -- single script input
        ; all (==(True :: Bool)) [a,b,c,d,e,f,g]
        }

      -- | A printer cancels a printing job and returns it to the job pool.
      printerCancelsJob :: Bool
      printerCancelsJob = do
        { let cdt = embeddedDatum scriptTxOutputs
        ; let a = traceIfFalse "Incorrect Token Cont" $ checkContTxOutForValue scriptTxOutputs tokenValue -- token must go back to script
        ; let b = traceIfFalse "Incorrect Signer"     $ checkTxSigner confirmedPrinterPKH                 -- the printer must sign it
        ; let c = traceIfFalse "Incorrect State"      $ cdtStateFlag   datum == aJobIsCurrentlyPrinting   -- the token is currently printing
        ; let d = traceIfFalse "Incorrect New State"  $ cdtStateFlag   cdt   == aCustomerHasAPrintingJob  -- the token is looking to be printed
        ; let e = traceIfFalse "Incorrect Customer"   $ cdtCustomerPKH cdt   == customerPKH               -- customer remains the same
        ; let f = traceIfFalse "Incorrect Price"      $ cdtPrice       cdt   == cdtPrice datum            -- price remains the same
        ; let g = traceIfFalse "Too Many Script Inputs" checkForSingleScriptInput                         -- single script input
        ; all (==(True :: Bool)) [a,b,c,d,e,f,g]
        }

      -- | A printer finishes a printing job and ships the item.
      printerFinishesJob :: Bool
      printerFinishesJob = do
        { let cdt = embeddedDatum scriptTxOutputs
        ; let a = traceIfFalse "Incorrect Token Cont" $ checkContTxOutForValue scriptTxOutputs tokenValue -- token must go back to script
        ; let b = traceIfFalse "Incorrect Signer"     $ checkTxSigner confirmedPrinterPKH                 -- printer must sign
        ; let c = traceIfFalse "Incorrect State"      $ cdtStateFlag   datum == aJobIsCurrentlyPrinting   -- job is printing
        ; let d = traceIfFalse "Incorrect New State"  $ cdtStateFlag   cdt   == aJobIsCurrentlyShipping   -- job is done printing and is now shipping
        ; let e = traceIfFalse "Incorrect Customer"   $ cdtCustomerPKH cdt   == customerPKH               -- customer remains the same
        ; let f = traceIfFalse "Incorrect Price"      $ cdtPrice       cdt   == cdtPrice datum            -- price remains the same
        ; let g = traceIfFalse "Incorrect Printer"    $ cdtPrinterPKH  cdt   == confirmedPrinterPKH       -- printer remains the same
        ; let h = traceIfFalse "Incorrect Print Time" $ cdtPrintTime   cdt   == cdtPrintTime datum        -- print time remains the same
        ; let i = traceIfFalse "Too Many Script Inputs" checkForSingleScriptInput                         -- single script input
        ; all (==(True :: Bool)) [a,b,c,d,e,f,g,h,i]
        }


      -- | A customer receives the item and confirms the item is ok.
      -- This connects the real world to the contract.
      customerConfirmsJob :: Bool
      customerConfirmsJob = do
        { let a = traceIfFalse "Incorrect State"        $ cdtStateFlag datum == aJobIsCurrentlyShipping                           -- job is shipping
        ; let b = traceIfFalse "Incorrect Signer"       $ checkTxSigner customerPKH                                               -- customer must sign it
        ; let c = traceIfFalse "Incorrect Token payout" $ checkTxOutForValueAtPKH currentTxOutputs customerPKH tokenValue         -- customer gets token back
        ; let d = traceIfFalse "Incorrect Price payout" $ checkTxOutForValueAtPKH currentTxOutputs confirmedPrinterPKH priceValue -- printer gets paid
        ; let e = traceIfFalse ""                       True                                                                      -- Proof that item was received.
        ; let f = traceIfFalse "Too Many Script Inputs" checkForSingleScriptInput                                                 -- single script input
        ; all (==(True :: Bool)) [a,b,c,d,e,f]
        }

      -------------------------------------------------------------------------
      -- | TxInfo and TxOut Here
      -------------------------------------------------------------------------

      info :: TxInfo
      info = scriptContextTxInfo context

      -- All the current outputs.
      currentTxOutputs :: [TxOut]
      currentTxOutputs = txInfoOutputs info

      -- All the outputs going back to the script.
      scriptTxOutputs  :: [TxOut]
      scriptTxOutputs  = getContinuingOutputs context

       -------------------------------------------------------------------------
      -- | Different Types of Sale Data Here
      -------------------------------------------------------------------------

      -- pubkeys
      customerPKH :: PubKeyHash
      customerPKH = cdtCustomerPKH datum

      confirmedPrinterPKH :: PubKeyHash
      confirmedPrinterPKH = cdtPrinterPKH datum

      potentialPrinterPKH :: PubKeyHash
      potentialPrinterPKH = crtPrinterPKH redeemer

      -- Values
      priceValue :: Value
      priceValue = Ada.lovelaceValueOf $ cdtPrice datum

      tokenValue :: Value
      tokenValue = case findOwnInput context of
        Nothing     -> traceError "No Input to Validate."  -- This should never be hit.
        Just input  -> txOutValue $ txInInfoResolved input

      -- this should always fail
      badCdt :: CustomDatumType
      badCdt = CustomDatumType {cdtPrice=0,cdtCustomerPKH=emptyPKH,cdtPrinterPKH=emptyPKH,cdtStateFlag=999999,cdtPrintTime=0}
        where
          emptyPKH :: PubKeyHash
          emptyPKH = PubKeyHash { getPubKeyHash="" }

      -- Find the new datum or return the old datum
      embeddedDatum :: [TxOut] -> CustomDatumType
      embeddedDatum [] = badCdt
      embeddedDatum (x:xs) = case txOutDatumHash x of
        Nothing -> embeddedDatum xs
        Just dh -> case findDatum dh info of
          Nothing        -> badCdt
          Just (Datum d) -> Data.Maybe.fromMaybe badCdt (PlutusTx.fromBuiltinData d)

      -------------------------------------------------------------------------
      -- | Check Some Condition Functions Here
      -------------------------------------------------------------------------
      -- | Check if a signee has signed the pending transaction.
      checkTxSigner :: PubKeyHash -> Bool
      checkTxSigner signee = txSignedBy info signee

      -- Search each TxOut for the correct address and value.
      checkTxOutForValueAtPKH :: [TxOut] -> PubKeyHash -> Value -> Bool
      checkTxOutForValueAtPKH [] _pkh _val = False
      checkTxOutForValueAtPKH (x:xs) pkh val
        | checkAddr && checkVal = True
        | otherwise             = checkTxOutForValueAtPKH xs pkh val
        where
          checkAddr :: Bool
          checkAddr = txOutAddress x == pubKeyHashAddress pkh

          checkVal :: Bool
          checkVal = txOutValue x == val

      -- | Search each TxOut for the value.
      checkContTxOutForValue :: [TxOut] -> Value -> Bool
      checkContTxOutForValue [] _val = False
      checkContTxOutForValue (x:xs) val
        | checkVal  = True
        | otherwise = checkContTxOutForValue xs val
        where
          checkVal :: Bool
          checkVal = txOutValue x == val
      
      -- Force a single script utxo input.
      checkForSingleScriptInput :: Bool
      checkForSingleScriptInput = loopInputs (txInfoInputs info) 0
        where
          loopInputs :: [TxInInfo] -> Integer -> Bool
          loopInputs []     !counter = counter == 1
          loopInputs (x:xs) !counter = case txOutDatumHash $ txInInfoResolved x of
              Nothing -> do
                if counter > 1
                  then loopInputs [] counter
                  else loopInputs xs counter
              Just _  -> do
                if counter > 1
                  then loopInputs [] counter
                  else loopInputs xs (counter + 1)
      
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

typedValidator :: ContractParams -> Scripts.TypedValidator Typed
typedValidator cp = Scripts.mkTypedValidator @Typed
  ($$(PlutusTx.compile [|| mkValidator ||]) `PlutusTx.applyCode` PlutusTx.liftCode cp)
  $$(PlutusTx.compile  [|| wrap        ||])
    where
      wrap = Scripts.wrapValidator @CustomDatumType @CustomRedeemerType  -- @Datum @Redeemer


-------------------------------------------------------------------------------
-- | The code below is required for the plutus script compile.
-------------------------------------------------------------------------------

script :: Plutus.Script
script = Plutus.unValidatorScript validator

printingPoolScriptShortBs :: SBS.ShortByteString
printingPoolScriptShortBs = SBS.toShort . LBS.toStrict $ serialise script

printingPoolScript :: PlutusScript PlutusScriptV1
printingPoolScript = PlutusScriptSerialised printingPoolScriptShortBs


type Schema =
  Endpoint ""  ()

contract :: AsContractError e => Contract () Schema e ()
contract = selectList [] >> contract

