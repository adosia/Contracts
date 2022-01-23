# Smart Contracts

A Collection of Smart Contracts for Adosia

# plutus Starter Setup
```bash
cabal clean
cabal build
cabal exec -- plutus-starter-pab
```

# Basic Start Up

Have a cardano-node inside the folder
Starting the testnet node.
```bash
cd cardano-node
./testnet-node-local/bin/cardano-node-testnet
```

Getting the node socket on path.

```bash
cd bash_scripts
CARDANO_NODE_SOCKET_PATH=../../cardano-node/state-node-testnet/node.socket
../../cardano-node/cardano-cli-testnet/bin/cardano-cli query tip --testnet-magic 1097911063
```

Compile the Haskell code. The example below is for the vesting contract.

```bash
cabal clean
cabal build -w ghc-8.10.4
cabal run space-printer-marketplace
echo "done"
```

# PAB Playground Setup

Dependent upon the nix environment. Requires Plutus to be in the folder.

This requires two terminals to use.

```bash
# First Terminal
cd plutus
git checkout plutus-pab/v0.0.2
. /home/cardano/.nix-profile/etc/profile.d/nix.sh
nix-shell
cd plutus-playground-client
. /home/cardano/.nix-profile/etc/profile.d/nix.sh
plutus-playground-server

# Second Terminal
cd plutus
git checkout plutus-pab/v0.0.2
. /home/cardano/.nix-profile/etc/profile.d/nix.sh
nix-shell
cd plutus-playground-client
. /home/cardano/.nix-profile/etc/profile.d/nix.sh
npm run start
```

# Get Node and Plutus

[Building the node with Nix](https://github.com/input-output-hk/cardano-node/blob/master/doc/getting-started/building-the-node-using-nix.md/)

```bash
cd cardano-node
git fetch --all --recurse-submodules --tags
git checkout $(curl -s https://api.github.com/repos/input-output-hk/cardano-node/releases/latest | jq -r .tag_name)
. /home/cardano/.nix-profile/etc/profile.d/nix.sh
nix-build -A scripts.testnet.node -o testnet-node-local
nix-build -A cardano-cli -o cardano-cli-testnet
./cardano-cli-testnet/bin/cardano-cli --version
./testnet-node-local/bin/cardano-node-testnet
```

### Be sure to set up a nix binary cache.
```bash
git clone https://github.com/input-output-hk/cardano-node
cd cardano-node
nix-build -A scripts.testnet.node -o testnet-node-local
```

```bash
git clone https://github.com/input-output-hk/plutus.git
```
