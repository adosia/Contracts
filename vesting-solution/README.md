# Project Catalyst Fund 7 Vesting Contract

Each contract is compiled specifically for a single token vestment solution. This allows a single contract to handle an entire vesting group where each utxo represents a member of the vesting group. This logic allows for a nice seperation of assets and amazing customization on vesting user level. The contract provides the functionality for linear vesting with weighted multisig on-chain voting. Inside this contract, the right to vest and vote is determined by public key hashes. This means the representation of a user's right to vest can not be traded.

This contract is designed for a service provider to handle all UI/UX and receive payment for their services. The scripts in this repository allow for basic cli/web functionality for testing purposes only and will need to expanded upon for production environments. Please use the current release for the most stable version of the smart contract.

The project is being released under Apache License 2.0 (Apache-2.0) so you can do what you like with this repo, as long as you include the required notices. If the contract gains traction post release then I will continue to contribute else it will end after the fund7 completion date, June 3rd 2022.

## Parameter Customization

Each vesting contract has a contract parameter structure that is defined at compile time and can not be changed in the future. These are the global parameters for every UTxO inside the contract. These values are required and must be agreed upon by the vesting group before the vesting contract is compiled.

```hs
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
  -- ^ Provider Profit in lovelaces, must be >= 1 ADA.
  }
```

The majority parameter, vcMajorityParam, is one of the most important parameter for the vesting group. This integer determines the total voting weight required for a vote will pass. It is typically defined between zero and one hundred. Since the value is defined at compile time, a vesting group may circumvent the value with a special type of vote the rescales the voting weight. This customizable behavior via vote will be applied in many aspects of this vesting solution.

The smart contract is designed for a singular policy token. It will work with either non-fungible and fungible tokens. If a vesting group requries multiple tokens to be vested then multiple contracts can be compiled for that specific vesting group. 

The provider profit is placed inside the contracts parameters to prevent the vesting group from voting that the profit parameter is removed. The contract is designed for some a service provider to be paid for providing the ability to have a fully on-chain vesting solution.

### Voting Example

There are five voters inside the vesting group. Each voter has a voting weight attached to their payment public key hash inside the contract. The sum of all the voting weight is 100 and the majority parameter will equal 65.

Their weights are:

```js
{   0: 23
,   1: 17
,   2: 8
,   3: 34
,   4: 18
}
```

A successful vote here is any combination of signatures that sum to a weight greater than or equal to the majority paramter, 65. In this case, there are 10 possible combinations for a successful vote. Each number corresponds to a voter's signature on a transaction that represents a vote. A voter answers YES when they sign a vote transaction. Not signing is abstaining which is a vote of NO. The order of the voters in each solution does not matter.

Successful Votes:

```js
[1, 3, 4]
[1, 2, 3, 4]
[0, 3, 4]
[0, 2, 3]
[0, 2, 3, 4]
[0, 1, 3]
[0, 1, 3, 4]
[0, 1, 2, 4]
[0, 1, 2, 3]
[0, 1, 2, 3, 4]
```

This method of voting can be advantagous because all possible outcomes must be calculated. There is no ambiguity. This potential benefit becomes an issue within this system because it does not scale well with a large group. Each member is required to sign on the order of 2^N transactions, where N is the number of voters in the group. This problem becomes unweldly quickly.

A simple solution for scaling that maintains the untradability of the right to vest and vote is voting delegation via multisig wallets. Advanced solutions may involve designing a singluar signature signing scheme for sets of transactions or GPU wallet signing nodes that brute force all the transactions. Both are well above the specifications of this proposal.

The vesting group information is stored inside the datum of a single vestor's vesting UTxO. This is done on purpose such that members of the vesting and voting group could be removed. A member being removed from the entire contract means they must be removed from every single voting group in each UTxO. This is why the endpoint is very open ended and only requires a successful vote. It is on the duty of the voter to inspect the voting transaction before signing.


## UTxO Level Customization

Each UTxO inside the contract hash a specific type of datum that controls the entire vesting solution for that user. The datum is allowed to change via a successfull vest or vote.

```hs
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
  -- ^ The reward function parameters [deltaV, v0]
  }
```

### Deadline Example

The contract has a time unit of days. A vestment starts at t0 days from the turn of epoch 312, which is about Jan 1 2022. Each vestment is given out to a vestor at some fix interval, deltaT. For example, a vestment contract can start on Jan 5 2022, t0=4, and gives out rewards every 5 days, deltaT=5, then my deadline parameters would be a list of [5, 4]. When a vestor successfull retrieves their vestment for the vesting period the deadline parameters are updated by adding deltaT to t0 and creating a new deadline parameter from the summation, [5, 9]. This process will repeat until all the funds have been retrieved or the vestor has been removed from the contract.

If the vesting period needs to be dynamic then the vestor or service provider can petition a vote to change the vestors deadline parameters. A lot of dynamic functionality can be mimicked using the voting endpoint. It can be a very useful tool for small changes to a single vestors vestment while not change anything in the global vesting contract itself. 

The vesting payouts do roll over due to the style of the deadline parameters in this contract. Vesting groups can vote to change the deadline parameters such that a vestor is skipped if they please.

### Reward Example

The reward parameters allow for a constant or linearly decreasing reward function. Each reward payout follow the equation r = v0 - t * deltaV, where t is the cdtVestingStage parameter in the datum. This is the number of times the vestor has retrieved a vestment. The term v0 is the intial value of the reward and deltaV is the decrease to the reward over time. Some initial calculations must be done to ensure correct parameters but of course a successful vote can correct a bad reward function. The function was chosen to model a typical return curve that decays over time but still allows for a user to get constant rewards if required by the vesting group. 

Similar to the deadline parameters, any non-linear behavior must be obtained with the voting endpoint.

## Clone The Repo

The vesting solution is contained in the Adosia Contracts repository. Clone the repo and checkout the most recent tagged release. This will be the stable version of every contract in that repository. The repo is split into folders of different types of decentralized applications. Advance users wishing to clone just a sub-folder are suggested to follow [this simple guide](https://dev.to/kiwicopple/quick-tip-clone-a-single-folder-from-github-44h6) on performing a sparse checkout on the repository.

```bash
git clone https://github.com/adosia/Contracts.git
cd Contracts
git fetch --all --recurse-submodules --tags
git checkout $(curl -s https://api.github.com/repos/adosia/Contracts/releases/latest | jq -r .tag_name)
cd vesting-solution
```

## Building

The contract can be compiled with cabal. Please see the requirements section for recommended versions of cabal and ghc. Compiling the contract can be a time intensive task for low core / low ram systems. Users can still confirm the validity of the precompiled plutus script without compiling the Haskell by building the smart contract address with the cli and confirming with others that the contract is legitimate.


Compiling Plutus:

```bash
# Clean and Build, this take a long time.
cd vesting-contract
rm vesting_contract.plutus
cabal clean
cabal build -w ghc-8.10.4
cabal run vesting-contract
echo "done"
```

Building An Address On Testnet:

```bash
cd vesting-contract
cardano-cli address build --payment-script-file vesting_contract.plutus --testnet-magic 1097911063
```

Building An Address On Mainnet:

```bash
cd vesting-contract
cardano-cli address build --payment-script-file vesting_contract.plutus --mainnet
```

The compiled contract address is dependent upon the network being used. Please refer to the addresses below for official addresses.

```
# testnet
addr_test1wzema20akaeh6ulc55zzklsqmuktlxngqqaz7vj5jnvu2dsujlk7x

# mainnet
addr1wxema20akaeh6ulc55zzklsqmuktlxngqqaz7vj5jnvu2ds86t23r
```

### Testnet / Mainnet Adjustment

The contract is designed for use on the testnet and mainnet but the reference epoch is dependent on network. The transition into epoch 312 happened on testnet first then on mainnet. When compiling for mainnet be sure to adjust the hardcoded reference time. This change is in the HelperFuncs.hs file in the lockInterval function. Without this required change, mainnet time caluclations will be off by about a day.

```hs
    -- unix time at epoch 312
    timeTilRefEpoch :: Integer
    -- timeTilRefEpoch = 1640987100000  -- mainnet
    timeTilRefEpoch = 1640895900000  -- testnet
```

## Requirements

```
cardano-cli --version
# cardano-cli 1.34.1 - linux-x86_64 - ghc-8.10
# git rev 73f9a746362695dc2cb63ba757fbcabb81733d23

cardano-node --version
# cardano-node 1.34.1 - linux-x86_64 - ghc-8.10
# git rev 73f9a746362695dc2cb63ba757fbcabb81733d23

cabal --version
# cabal-install version 3.4.0.0
# compiled using version 3.4.0.0 of the Cabal library

ghc --version
# The Glorious Glasgow Haskell Compilation System, version 8.10.4

python --version
# Python 3.9.5
```

## Links

[A Community Vesting Dapp](https://cardano.ideascale.com/c/idea/382448)

[GitBook](https://logicalmechanism.gitbook.io/open-source-vesting-contract/)

[Github](https://github.com/adosia/Contracts)

[Lucid Charts](https://lucid.app/lucidchart/ae7436f3-ce4f-499a-ab9d-da7002b6ec8e/edit?invitationId=inv_bba2c9a6-875d-46e8-954e-906379abff82)