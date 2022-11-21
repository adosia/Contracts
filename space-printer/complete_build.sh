#!/bin/bash
set -e

# constant variables
testnet_magic=2

echo -e "\033[1;35m \nPlacing Starter NFT Into Design Contracts. \033[0m"

python3 -c "import binascii;a=$(cat start_info.json | jq .starterPid);s=binascii.unhexlify(a);print([x for x in s])" > starter.pid
python3 -c "import binascii;a=$(cat start_info.json | jq .starterTkn);s=binascii.unhexlify(a);print([x for x in s])" > starter.tkn

# Adds in the locking token into the contract.
python3 -c "from update_contracts import changeStartPid;changeStartPid('./design-locking-contract/src/LockingContract.hs', './design-locking-contract/src/LockingContract-new.hs', $(cat starter.pid))"
mv ./design-locking-contract/src/LockingContract-new.hs ./design-locking-contract/src/LockingContract.hs
python3 -c "from update_contracts import changeStartTkn;changeStartTkn('./design-locking-contract/src/LockingContract.hs', './design-locking-contract/src/LockingContract-new.hs', $(cat starter.tkn))"
mv ./design-locking-contract/src/LockingContract-new.hs ./design-locking-contract/src/LockingContract.hs

python3 -c "from update_contracts import changeStartPid;changeStartPid('./design-minting-contract/src/MintingContract.hs', './design-minting-contract/src/MintingContract-new.hs', $(cat starter.pid))"
mv ./design-minting-contract/src/MintingContract-new.hs ./design-minting-contract/src/MintingContract.hs
python3 -c "from update_contracts import changeStartTkn;changeStartTkn('./design-minting-contract/src/MintingContract.hs', './design-minting-contract/src/MintingContract-new.hs', $(cat starter.tkn))"
mv ./design-minting-contract/src/MintingContract-new.hs ./design-minting-contract/src/MintingContract.hs

echo -e "\033[1;35m \nBuilding Design Locking Contract. \033[0m"

cd design-locking-contract

cabal build -w ghc-8.10.7 -O2
cabal run design-locking-contract

cardano-cli address build --payment-script-file locking-contract.plutus --testnet-magic ${testnet_magic} --out-file validator.addr
cardano-cli transaction policyid --script-file locking-contract.plutus > validator.hash
python3 -c "import binascii;a='$(cat validator.hash)';s=binascii.unhexlify(a);print([x for x in s])" > validator.bytes


echo 
echo -e "\033[1;33m Validator Address: $(cat validator.addr) \033[0m" 
echo -e "\033[1;33m Validator Hash: $(cat validator.hash) \033[0m" 
echo -e "\033[1;33m Validator Bytes: $(cat validator.bytes) \033[0m" 

cd ..


echo -e "\033[1;35m \nPlacing Design Validator hash Into Design Mint Contract. \033[0m"

# adds in the locking hash into the script
python3 -c "from update_contracts import changeDesignLockHash;changeDesignLockHash('./design-minting-contract/src/MintingContract.hs', './design-minting-contract/src/MintingContract-new.hs', $(cat ./design-locking-contract/validator.bytes))"
mv ./design-minting-contract/src/MintingContract-new.hs ./design-minting-contract/src/MintingContract.hs


echo -e "\033[1;35m \nBuilding Design Minting Contract. \033[0m"

cd design-minting-contract

cabal build -w ghc-8.10.7 -O2
cabal run design-minting-contract

cardano-cli transaction policyid --script-file minting-contract.plutus > policy.id
python3 -c "import binascii;a='$(cat policy.id)';s=binascii.unhexlify(a);print([x for x in s])" > policy.bytes

# nft minting info
echo
echo -e "\033[1;33m Policy Id: $(cat policy.id) \033[0m"
echo -e "\033[1;33m Policy Bytes: $(cat policy.bytes) \033[0m"

cd ..

variable=$(cat ./design-minting-contract/policy.id); jq --arg variable "$variable" '.invoicePid=$variable' ./start_info.json > ./start_info-new.json
mv ./start_info-new.json ./start_info.json

# end of design lock and mint build
#
# exit
#

echo -e "\033[1;35m \nPlacing Design Policy Id Into The Marketplace And Invoice Contract. \033[0m" 

# get info
designPidBytes=$(cat ./design-minting-contract/policy.bytes)

# adds in the locking hash into the script
python3 -c "from update_contracts import changeStartPid;changeStartPid('./marketplace-contract/src/MarketplaceContract.hs', './marketplace-contract/src/MarketplaceContract-new.hs', ${designPidBytes})"
mv ./marketplace-contract/src/MarketplaceContract-new.hs ./marketplace-contract/src/MarketplaceContract.hs

# adds in the locking hash into the script
python3 -c "from update_contracts import changeStartPid;changeStartPid('./invoice-minting/src/InvoiceMinting.hs', './invoice-minting/src/InvoiceMinting-new.hs', ${designPidBytes})"
mv ./invoice-minting/src/InvoiceMinting-new.hs ./invoice-minting/src/InvoiceMinting.hs

echo -e "\033[1;35m \nBuilding Marketplace Contract... \033[0m" 

cd marketplace-contract

cabal build -w ghc-8.10.7
cabal run marketplace-contract

cardano-cli address build --payment-script-file marketplace-contract.plutus --testnet-magic ${testnet_magic} --out-file validator.addr
cardano-cli transaction policyid --script-file marketplace-contract.plutus > validator.hash
python3 -c "import binascii;a='$(cat validator.hash)';s=binascii.unhexlify(a);print([x for x in s])" > validator.bytes

echo 
echo -e "\033[1;33m Validator Address: $(cat validator.addr) \033[0m" 
echo -e "\033[1;33m Validator Hash: $(cat validator.hash) \033[0m" 
echo -e "\033[1;33m Validator Bytes: $(cat validator.bytes) \033[0m" 


cd ..

echo -e "\033[1;35m \nPlacing Marketplace Script Hash Into Invoice Contract. \033[0m" 

# adds in the locking hash into the script
python3 -c "from update_contracts import changeLockHash;changeLockHash('./invoice-minting/src/InvoiceMinting.hs', './invoice-minting/src/InvoiceMinting-new.hs', $(cat ./marketplace-contract/validator.bytes))"
mv ./invoice-minting/src/InvoiceMinting-new.hs ./invoice-minting/src/InvoiceMinting.hs

cd invoice-minting

cabal build -w ghc-8.10.7
cabal run invoice-minting

cardano-cli transaction policyid --script-file minting-contract.plutus > policy.id
python3 -c "import binascii;a='$(cat policy.id)';s=binascii.unhexlify(a);print([x for x in s])" > policy.bytes

echo 
echo -e "\033[1;33m Policy Id: $(cat policy.id) \033[0m" 
echo -e "\033[1;33m Policy Bytes: $(cat policy.bytes) \033[0m" 

# update the register redeemer to put the stake key on chain
variable=$(cat policy.id); jq --arg variable "$variable" '.fields[4].bytes=$variable' ../scripts/marketplace-scripts/data/datum/token_sale_datum.json > ../scripts/marketplace-scripts/data/datum/token_sale_datum-new.json
mv ../scripts/marketplace-scripts/data/datum/token_sale_datum-new.json ../scripts/marketplace-scripts/data/datum/token_sale_datum.json

cd ..

# adds in the locking hash into the script
python3 -c "from update_contracts import changeStartLockPid;changeStartLockPid('./printing-pool/src/PrintingPool.hs', './printing-pool/src/PrintingPool-new.hs', $(cat ./invoice-minting/policy.bytes))"
mv ./printing-pool/src/PrintingPool-new.hs ./printing-pool/src/PrintingPool.hs

cd printing-pool

cabal build -w ghc-8.10.7
cabal run printing-pool

cardano-cli address build --payment-script-file printing-pool.plutus --testnet-magic ${testnet_magic} --out-file validator.addr
cardano-cli transaction policyid --script-file printing-pool.plutus > validator.hash
python3 -c "import binascii;a='$(cat validator.hash)';s=binascii.unhexlify(a);print([x for x in s])" > validator.bytes


echo 
echo -e "\033[1;33m Validator Address: $(cat validator.addr) \033[0m" 
echo -e "\033[1;33m Validator Hash: $(cat validator.hash) \033[0m" 
echo -e "\033[1;33m Validator Bytes: $(cat validator.bytes) \033[0m" 


echo "DONE"