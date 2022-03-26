# Basic Start Up

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
cabal run printing-pool
echo "done"
```