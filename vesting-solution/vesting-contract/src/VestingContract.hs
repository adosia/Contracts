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
  , contract
  , Schema
  , CustomDatumType
  ) where
import           Cardano.Api.Shelley       (PlutusScript (..), PlutusScriptV1)
import           Codec.Serialise           ( serialise )
import qualified Data.ByteString.Lazy      as LBS
import qualified Data.ByteString.Short     as SBS
import           Data.Maybe
import           Plutus.Contract
import           Ledger
import qualified Ledger.Typed.Scripts      as Scripts
import qualified PlutusTx
import           PlutusTx.Prelude
import qualified Plutus.V1.Ledger.Scripts  as Plutus
import qualified Plutus.V1.Ledger.Value    as Value
import qualified Plutus.V1.Ledger.Ada      as Ada
import           HelperFuncs
import           DataTypes
import           CheckFuncs
{- |
  Author   : The Ancient Kraken
  Copyright: 2022
  Version  : Rev 1

  This is a vesting solution. There are contract level and utxo level variables.
  The contract level is designed to hold unchangeable information that all utxos
  must obey. The utxo level is more individual vestor focused, holding information
  that may vary from vestor to vestor. This split allows for a lot of unique
  features for a vesting group. There are three on-chain validation endpoints. A
  user may either retrieve their funds, close their finished vestment, or petition
  a vote. Voting is a weighted multisig agreement between members of a utxo's
  voting group. Anything can be changed but contract level variables.
-}
-------------------------------------------------------------------------------
-- | Create the token sale parameters data object.
-------------------------------------------------------------------------------
data VestingContractParams = VestingContractParams
  { vcMajorityParam  :: !Integer
  -- ^ Threshold weight to determine majority.
  , vcPolicyID       :: !CurrencySymbol
  -- ^ The policy id of the vesting token.
  , vcTokenName      :: !TokenName
  -- ^ The token name of the vesting token.
  , vcProviderPKH    :: !PubKeyHash
  -- ^ The vesting as a service provider pkh.
  , vcProviderProfit :: !Integer
  -- ^ Provider Profit in lovelaces.
  }
PlutusTx.makeLift ''VestingContractParams
-------------------------------------------------------------------------------
-- | Create the redeemer parameters data object.
-------------------------------------------------------------------------------
data CustomRedeemerType = RetrieveFunds | CloseVestment | PetitionVote
PlutusTx.makeIsDataIndexed ''CustomRedeemerType [ ('RetrieveFunds, 0)
                                                , ('CloseVestment, 1)
                                                , ('PetitionVote,  2)
                                                ]
PlutusTx.makeLift ''CustomRedeemerType
-------------------------------------------------------------------------------
-- | mkValidator :: Data -> Datum -> Redeemer -> ScriptContext -> Bool
-------------------------------------------------------------------------------
{-# INLINABLE mkValidator #-}
mkValidator :: VestingContractParams -> CustomDatumType -> CustomRedeemerType -> ScriptContext -> Bool
mkValidator vc datum redeemer context =
  case redeemer of
    RetrieveFunds -> do
      { let a = traceIfFalse "Single Script Only"           $ nInputs == (1 :: Integer)
      ; let b = traceIfFalse "Incorrect Signer"             $ txSignedBy info vestingUser
      ; let c = traceIfFalse "The Value Is Still Locked"    $ not $ overlaps lockedInterval validityInterval
      ; let d = traceIfFalse "Incorrect Incoming Datum"     $ datum == embeddedDatum datum info contTxOutputs
      ; let e = traceIfFalse "Value Not Return To Script"   $ checkContTxOutForValue contTxOutputs (validatedValue - retrieveValue)
      ; let f = traceIfFalse "Funds Not Being Retrieved"    $ checkTxOutForValueAtPKH txOutputs vestingUser retrieveValue
      ; let g = traceIfFalse "No Funds Left To Vest"        $ Value.valueOf validatedValue policyId tokenName > (0 :: Integer)
      ; let h = traceIfFalse "Provider Not Being Paid"      $ checkTxOutForValueAtPKH txOutputs providerPKH profitValue
      ;         traceIfFalse "Error: retrieveFunds Failure" $ all (==(True :: Bool)) [a,b,c,d,e,f,g,h]
      }
    CloseVestment -> do
      { let a = traceIfFalse "Single Script Only"           $ nInputs == (1 :: Integer)
      ; let b = traceIfFalse "Funds Not Being Retrieved"    $ checkTxOutForValueAtPKH txOutputs treasuryPKH validatedValue
      ; let c = traceIfFalse "Funds Are Left To Vest"       $ Value.valueOf validatedValue policyId tokenName == (0 :: Integer)
      ;         traceIfFalse "Error: closeVestment Failure" $ all (==(True :: Bool)) [a,b,c]
      }
    PetitionVote -> do
      { let a = traceIfFalse "Not Enough Signers"          $ checkVoteWeight info datum (vcMajorityParam vc)
      ; let b = traceIfFalse "Provider Not Being Paid"     $ checkTxOutForValueAtPKH txOutputs providerPKH profitValue
      ;         traceIfFalse "Error: petitionVote Failure" $ all (==(True :: Bool)) [a,b]
      }
  where
    -------------------------------------------------------------------------
    -- | Helper Variables
    -------------------------------------------------------------------------
    info :: TxInfo
    info = scriptContextTxInfo context

    contTxOutputs :: [TxOut]
    contTxOutputs = getContinuingOutputs context

    txOutputs :: [TxOut]
    txOutputs = txInfoOutputs info

    txInputs :: [TxInInfo]
    txInputs = txInfoInputs info

    nInputs :: Integer
    nInputs = checkForNScriptInputs txInputs

    lockedInterval :: Interval POSIXTime
    lockedInterval = lockInterval datum

    validityInterval :: POSIXTimeRange
    validityInterval = txInfoValidRange info

    vestingUser :: PubKeyHash
    vestingUser = cdtVestingUserPKH datum

    treasuryPKH :: PubKeyHash
    treasuryPKH = cdtTreasuryPKH datum

    policyId :: CurrencySymbol
    policyId = vcPolicyID vc

    tokenName :: TokenName
    tokenName = vcTokenName vc

    providerPKH :: PubKeyHash
    providerPKH = vcProviderPKH vc

    profitValue :: Value
    profitValue = Ada.lovelaceValueOf $ vcProviderProfit vc * nInputs

    validatedValue :: Value
    validatedValue =
      case findOwnInput context of
        Nothing    -> traceError "No Input to Validate"
        Just input -> txOutValue $ txInInfoResolved input

    retrieveValue :: Value
    retrieveValue = Value.singleton policyId tokenName (rewardFunction datum)
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
   $$(PlutusTx.compile [|| wrap        ||])
    where
      wrap = Scripts.wrapValidator @CustomDatumType @CustomRedeemerType  -- @Datum @Redeemer
-------------------------------------------------------------------------------
-- | Define The Token Sale Parameters Here
-------------------------------------------------------------------------------
validator :: Plutus.Validator
validator = Scripts.validatorScript (typedValidator vc)
  where
    vc = VestingContractParams
      { vcMajorityParam  = 50
      , vcPolicyID       = "57fca08abbaddee36da742a839f7d83a7e1d2419f1507fcbf3916522"
      , vcTokenName      = "CHOC"
      , vcProviderPKH    = "06c35b3567b2d8f4c3a838c44050fa785c702d532467c8bfdb85046b"
      , vcProviderProfit = 1000000
      }
-------------------------------------------------------------------------------
-- | The code below is required for the plutus script compile.
-------------------------------------------------------------------------------
script :: Plutus.Script
script =  Plutus.unValidatorScript validator

vestingContractScriptShortBs :: SBS.ShortByteString
vestingContractScriptShortBs =  SBS.toShort . LBS.toStrict $ serialise script

vestingContractScript :: PlutusScript PlutusScriptV1
vestingContractScript =  PlutusScriptSerialised vestingContractScriptShortBs
-------------------------------------------------------------------------------
-- | Off Chain
-------------------------------------------------------------------------------
type Schema = Endpoint "" ()
contract :: AsContractError e => Contract () Schema e ()
contract = selectList [] >> contract