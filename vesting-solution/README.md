# Project Catalyst Fund 7 Vesting Contract

Each contract is compiled specifically for a token vestment solution. This contract is designed for a service provider to handle all UI/UX and receive payment for their services. The scripts in this repository allow for basic cli/web functionality for testing purposes only.

## Customization

Each vesting contract has a predefine parameter structure that can not be changed. These are parameters that are withheld for every UTxO inside the contract and require the vesting group to agree before the vesting contract is compiled.

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
  -- ^ Provider Profit in lovelaces.
  }
```

The majority parameter, vcMajorityParam, is one of the most important parameters for the vesting group. These number determines if a vote will pass or fail. The contract is designed for a singular policy vesting solution. If a vesting group requries multiple tokens then multiple contracts can be compiled. The provider profit is placed inside the contracts parameters to prevent the vesting group from voting that the profit parameter is removed. The contract is designed for some a service provider to be paid for providing the ability to have a fully on-chain vesting solution with voting.

## Vote Example

Each voter has a voting weight attached to their payment public key hash inside the contract. The voting weights sum to 100. 

In this contract the majority parameter is defined to be 65. There are five voters labeled with integers. 

Their weights are:

```js
{   0: 23
,   1: 17
,   2: 8
,   3: 34
,   4: 18
}
```

A successful vote here is any combination of signatures that sum to a weight greater than or equal to the majority paramter, 65. In this case, there are 10 possible combinations for a successful vote. Each number corresponds to a voter's signature on a transaction that represents a vote. A voter answers YES when they sign a vote transaction. Abstaining from the vote is NO. The order of the voters in each solution does not matter.

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

This method of voting is advantagous because it is known a priori all possible successful paths. There is no ambiguity. An issue with this system is that it does not scale well with a large group. Each member is required to sign on the order of 2^N transactions where N is the number of voters in the group. This problem becomes unweldly quickly. A simple solutions for large voting groups is delegation via multisig wallets or with a trusted service provider. 


## Building

The contract can be compiled with cabal. Please see the requirements section for recommended versions.

```bash
# Clean and Build, this take a long time.
rm vesting_contract.plutus
cabal clean
cabal build -w ghc-8.10.4
cabal run vesting-contract
echo "done"
```

## Requirements

```
cabal --version
# cabal-install version 3.4.0.0
# compiled using version 3.4.0.0 of the Cabal library

ghc --version
# The Glorious Glasgow Haskell Compilation System, version 8.10.4
```

## Links

[A Community Vesting Dapp](https://cardano.ideascale.com/c/idea/382448)

[GitBook](https://logicalmechanism.gitbook.io/open-source-vesting-contract/)

[Github](https://github.com/adosia/Contracts)

[Lucid Charts](https://lucid.app/lucidchart/ae7436f3-ce4f-499a-ab9d-da7002b6ec8e/edit?invitationId=inv_bba2c9a6-875d-46e8-954e-906379abff82)