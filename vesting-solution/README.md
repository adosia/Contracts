# Project Catalyst Fund 7 Vesting Contract

```bash
# Clean and Build, this take a long time.
rm vesting_contract.plutus
cabal clean
cabal build -w ghc-8.10.4
cabal run vesting-contract
echo "done"
```

```bash
# Quick Build
cabal build -w ghc-8.10.4
echo "done"
```
## Requirements

```
cardano-cli --version
# cardano-cli 1.33.0 - linux-x86_64 - ghc-8.10
# git rev 814df2c146f5d56f8c35a681fe75e85b905aed5d

cardano-node --version
# cardano-node 1.33.0 - linux-x86_64 - ghc-8.10
# git rev 814df2c146f5d56f8c35a681fe75e85b905aed5d

cabal --version
# cabal-install version 3.4.0.0
# compiled using version 3.4.0.0 of the Cabal library

ghc --version
# The Glorious Glasgow Haskell Compilation System, version 8.10.4

bash --version
# GNU bash, version 5.0.17(1)-release (x86_64-pc-linux-gnu)
```

## Links

[A Community Vesting Dapp](https://cardano.ideascale.com/c/idea/382448)

[GitBook](https://logicalmechanism.gitbook.io/open-source-vesting-contract/)

[Github](https://github.com/adosia/Contracts)

[Lucid Charts](https://lucid.app/lucidchart/ae7436f3-ce4f-499a-ab9d-da7002b6ec8e/edit?invitationId=inv_bba2c9a6-875d-46e8-954e-906379abff82)