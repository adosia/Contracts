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
  , CustomDatumType
  , test1
  , test2
  , test3
  , (===)
  ) where

import           Cardano.Api.Shelley       (PlutusScript (..), PlutusScriptV1)

import           Codec.Serialise           ( serialise )

import qualified Data.ByteString.Lazy      as LBS
import qualified Data.ByteString.Short     as SBS
import qualified Data.Maybe

import           Ledger                    hiding ( singleton )
import qualified Ledger.Typed.Scripts      as Scripts
import           Playground.Contract


import qualified PlutusTx
import           PlutusTx.Prelude
import           Plutus.Contract

import qualified Plutus.V1.Ledger.Ada      as Ada
import qualified Plutus.V1.Ledger.Scripts  as Plutus
import qualified Plutus.V1.Ledger.Value    as Value

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
  -- ^ The amount of lovelace the Printer will receive.
  , cdtCustomerPKH :: !PubKeyHash
  -- ^ The Seller's public key hash.
  , cdtPrinterPKH  :: !PubKeyHash
  -- ^ The Printer's public key hash.
  , cdtStateFlag   :: !Integer
  -- A state flag
  }
    deriving stock (Show, Generic)
    deriving anyclass (FromJSON, ToJSON, ToSchema)
PlutusTx.unstableMakeIsData ''CustomDatumType
PlutusTx.makeLift ''CustomDatumType

instance Eq CustomDatumType where
  {-# INLINABLE (==) #-}
  a == b = ( cdtPrice       a == cdtPrice       b) &&
           ( cdtCustomerPKH a == cdtCustomerPKH b) &&
           ( cdtPrinterPKH  a == cdtPrinterPKH  b) &&
           ( cdtStateFlag   a == cdtStateFlag b+1)

class Equiv a where
  (===) :: a -> a -> Bool
  (=/=) :: a -> a -> Bool
  (=//) :: a -> a -> Bool
  (///) :: a -> a -> Bool

instance Equiv CustomDatumType where
  -- the price needs to update
  {-# INLINABLE (===) #-}
  a === b = ( cdtPrice       b /= cdtPrice       a) &&
            ( cdtCustomerPKH a == cdtCustomerPKH b) &&
            ( cdtPrinterPKH  a == cdtPrinterPKH  b) &&
            ( cdtStateFlag   a == cdtStateFlag   b)
  
  -- customer denies / printer cancels
  {-# INLINABLE (=//) #-}
  a =// b = ( cdtPrice       b == cdtPrice       a) &&
            ( cdtCustomerPKH a == cdtCustomerPKH b) &&
            ( cdtPrinterPKH  a /= cdtPrinterPKH  b) &&
            ( cdtStateFlag   a == cdtStateFlag b-1)
  
   -- printer cancels
  {-# INLINABLE (///) #-}
  a /// b = ( cdtPrice       b == cdtPrice       a) &&
            ( cdtCustomerPKH a == cdtCustomerPKH b) &&
            ( cdtPrinterPKH  a /= cdtPrinterPKH  b) &&
            ( cdtStateFlag   a == cdtStateFlag b-2)

  -- printer make offer
  {-# INLINABLE (=/=) #-}
  a =/= b = (cdtPrice        a == cdtPrice       b) &&
            ( cdtCustomerPKH a == cdtCustomerPKH b) &&
            ( cdtPrinterPKH  a /= cdtPrinterPKH  b) &&
            ( cdtStateFlag   a == cdtStateFlag b+1)

test1 :: CustomDatumType
test1 = CustomDatumType
  { cdtPrice = 1000000
  , cdtCustomerPKH = "aa"
  , cdtPrinterPKH = ""
  , cdtStateFlag = 0
  }

test2 :: CustomDatumType
test2 = CustomDatumType
  { cdtPrice = 5000000
  , cdtCustomerPKH = "aa"
  , cdtPrinterPKH = ""
  , cdtStateFlag = 0
  }

test3 :: CustomDatumType
test3 = CustomDatumType
  { cdtPrice = 1000000
  , cdtCustomerPKH = "aa"
  , cdtPrinterPKH = "aa"
  , cdtStateFlag = 1
  }
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
        { let a = traceIfFalse "Incorrect State"        $ cdtStateFlag datum == aCustomerHasAPrintingJob                       -- token is looking to be printed
        ; let b = traceIfFalse "Incorrect Signer"       $ checkTxSigner customerPKH                                            -- customer must sign it
        ; let c = traceIfFalse "Incorrect Token Return" $ checkTxOutForValueAtPKH currentTxOutputs customerPKH validatingValue -- token must go back to customer
        ; let d = traceIfFalse "Too Many Script Inputs"   checkForSingleScriptInput                                            -- single script input
        ; all (==(True :: Bool)) [a,b,c,d]
        }

      -- | A customer can change the price of a printing job inside the pool.
      customerUpdatesJob :: Bool
      customerUpdatesJob = do
        { let incomingDatum = embeddedDatum continuingTxOutputs
        ; let a = traceIfFalse "Incorrect State"        $ cdtStateFlag datum == aCustomerHasAPrintingJob             -- token is looking to be printed
        ; let b = traceIfFalse "Incorrect Signer"       $ checkTxSigner customerPKH                                  -- the customer signs it
        ; let c = traceIfFalse "Incorrect Token Cont"   $ checkContTxOutForValue continuingTxOutputs validatingValue -- it must go back to the script
        ; let d = traceIfFalse "Incorrect New State"    $ datum === incomingDatum                                    -- price change only
        ; let e = traceIfFalse "Too Many Script Inputs"   checkForSingleScriptInput                                  -- single script input
        ; all (==(True :: Bool)) [a,b,c,d,e]
        }


      -- -- | A printer selects a job from the job pool.
      -- -- This needs some zk thing to prove to the contract they are a printer.
      printerMakesJobOffer :: Bool
      printerMakesJobOffer = do
        { let incomingDatum = embeddedDatum continuingTxOutputs
        ; let a = traceIfFalse "Incorrect State"        $ cdtStateFlag datum == aCustomerHasAPrintingJob             -- the token is looking to be printed
        ; let b = traceIfFalse "Incorrect Signer"       $ checkTxSigner potentialPrinterPKH                          -- The printer must sign it
        ; let c = traceIfFalse "Incorrect Token Cont"   $ checkContTxOutForValue continuingTxOutputs validatingValue -- token must go back to script
        ; let d = traceIfFalse "Incorrect New State"    $ datum =/= incomingDatum                                    -- printer must change the datum 
        ; let e = traceIfFalse "Incorrect State"        $ cdtPrinterPKH incomingDatum == potentialPrinterPKH         -- make sure its in the new datum
        ; let f = traceIfFalse "Too Many Script Inputs"   checkForSingleScriptInput                                  -- single script input
        ; all (==(True :: Bool)) [a,b,c,d,e,f]
        }

      -- -- | A printer selects a job from the job pool.
      customerAcceptsOffer :: Bool
      customerAcceptsOffer = do
        { let incomingDatum = embeddedDatum continuingTxOutputs
        ; let a = traceIfFalse "Incorrect State"        $ cdtStateFlag datum == aPrinterIsMakingAnOffer              -- the token is getting an offer
        ; let b = traceIfFalse "Incorrect Signer"       $ checkTxSigner customerPKH                                  -- customer must sign it
        ; let c = traceIfFalse "Incorrect Token Cont"   $ checkContTxOutForValue continuingTxOutputs validatingValue -- token must go back to script
        ; let d = traceIfFalse "Incorrect Printer"      $ datum == incomingDatum                                     -- Only the stage can change
        ; let e = traceIfFalse "Too Many Script Inputs"   checkForSingleScriptInput                                  -- single script input
        ; all (==(True :: Bool)) [a,b,c,d,e]
        }

       -- -- | A printer selects a job from the job pool.
      customerDeniesOffer :: Bool
      customerDeniesOffer = do
        { let incomingDatum = embeddedDatum continuingTxOutputs
        ; let a = traceIfFalse "Incorrect State"        $ cdtStateFlag datum == aPrinterIsMakingAnOffer              -- the token is getting an offer
        ; let b = traceIfFalse "Incorrect Signer"       $ checkTxSigner customerPKH                                  -- customer must sign it
        ; let c = traceIfFalse "Incorrect Token Cont"   $ checkContTxOutForValue continuingTxOutputs validatingValue -- token must go back to script
        ; let d = traceIfFalse "Incorrect New State"    $ datum =// incomingDatum                                    -- Drop printer and go back to the printing pool
        ; let e = traceIfFalse "Too Many Script Inputs"   checkForSingleScriptInput                                  -- single script input
        ; all (==(True :: Bool)) [a,b,c,d,e]
        }

      -- | A printer cancels a printing job and returns it to the job pool.
      printerCancelsJob :: Bool
      printerCancelsJob = do
        { let incomingDatum = embeddedDatum continuingTxOutputs
        ; let a = traceIfFalse "Incorrect State"        $ cdtStateFlag datum == aJobIsCurrentlyPrinting              -- the token is currently printing
        ; let b = traceIfFalse "Incorrect Signer"       $ checkTxSigner confirmedPrinterPKH                          -- the printer must sign it
        ; let c = traceIfFalse "Incorrect Token Cont"   $ checkContTxOutForValue continuingTxOutputs validatingValue -- token must go back to script
        ; let d = traceIfFalse "Incorrect New State"    $ datum /// incomingDatum                                    -- send token back to looking to be printed
        ; let e = traceIfFalse "Too Many Script Inputs"   checkForSingleScriptInput                                  -- single script input
        ; all (==(True :: Bool)) [a,b,c,d,e]
        }

      -- | A printer finishes a printing job and ships the item.
      printerFinishesJob :: Bool
      printerFinishesJob = do
        { let incomingDatum = embeddedDatum continuingTxOutputs
        ; let a = traceIfFalse "Incorrect State"        $ cdtStateFlag   datum == aJobIsCurrentlyPrinting            -- job is printing
        ; let b = traceIfFalse "Incorrect Signer"       $ checkTxSigner confirmedPrinterPKH                          -- printer must sign
        ; let c = traceIfFalse "Incorrect Token Cont"   $ checkContTxOutForValue continuingTxOutputs validatingValue -- token must go back to script
        ; let d = traceIfFalse "Incorrect New State"    $ datum == incomingDatum                                     -- job is done printing and is now shipping
        ; let e = traceIfFalse "Too Many Script Inputs"   checkForSingleScriptInput                                  -- single script input
        ; all (==(True :: Bool)) [a,b,c,d,e]
        }


      -- | A customer receives the item and confirms the item is ok.
      customerConfirmsJob :: Bool
      customerConfirmsJob = do
        { let a = traceIfFalse "Incorrect State"        $ cdtStateFlag datum == aJobIsCurrentlyShipping                           -- job is shipping
        ; let b = traceIfFalse "Incorrect Signer"       $ checkTxSigner customerPKH                                               -- customer must sign it
        ; let c = traceIfFalse "Incorrect Token payout" $ checkTxOutForValueAtPKH currentTxOutputs customerPKH validatingValue    -- customer gets token back
        ; let d = traceIfFalse "Incorrect Price payout" $ checkTxOutForValueAtPKH currentTxOutputs confirmedPrinterPKH priceValue -- printer gets paid
        ; let e = traceIfFalse "Too Many Script Inputs" checkForSingleScriptInput                                                 -- single script input
        ; all (==(True :: Bool)) [a,b,c,d,e]
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
      continuingTxOutputs  :: [TxOut]
      continuingTxOutputs  = getContinuingOutputs context

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

      validatingValue :: Value
      validatingValue = case findOwnInput context of
        Nothing     -> traceError "No Input to Validate."  -- This should never be hit.
        Just input  -> txOutValue $ txInInfoResolved input

      -- return the new datum if exist else return the old datum
      embeddedDatum :: [TxOut] -> CustomDatumType
      embeddedDatum [] = datum
      embeddedDatum (x:xs) = case txOutDatumHash x of
        Nothing -> embeddedDatum xs
        Just dh -> case findDatum dh info of
          Nothing        -> datum
          Just (Datum d) -> Data.Maybe.fromMaybe datum (PlutusTx.fromBuiltinData d)

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
          checkVal = Value.geq (txOutValue x) val
      
      -- Force a single script utxo input.
      checkForSingleScriptInput :: Bool
      checkForSingleScriptInput = loopInputs (txInfoInputs info) 0
        where
          loopInputs :: [TxInInfo] -> Integer -> Bool
          loopInputs []      counter = counter == 1
          loopInputs (x:xs) !counter = case txOutDatumHash $ txInInfoResolved x of
              Nothing -> do counter <= 1 && loopInputs xs counter
              Just _  -> do counter <= 1 && loopInputs xs (counter + 1)
      
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

