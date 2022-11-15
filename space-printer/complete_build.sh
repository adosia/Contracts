#!/bin/bash
set -e

echo -e "\033[1;35m \nPlacing Design Policy Id Into The Marketplace And Invoice Contract. \033[0m" 
testnet_magic=2

# get info
starterPid=$(cat start_info.json | jq -r .starterPid)
starterPidBytes=$(python3 -c "import binascii;a='${starterPid}';s=binascii.unhexlify(a);print([x for x in s])")

# adds in the locking hash into the script
python3 -c "from update_contracts import changeStartPid;changeStartPid('./marketplace-contract/src/MarketplaceContract.hs', './marketplace-contract/src/MarketplaceContract-new.hs', ${starterPidBytes})"
mv ./marketplace-contract/src/MarketplaceContract-new.hs ./marketplace-contract/src/MarketplaceContract.hs

# adds in the locking hash into the script
python3 -c "from update_contracts import changeStartPid;changeStartPid('./invoice-minting/src/InvoiceMinting.hs', './invoice-minting/src/InvoiceMinting-new.hs', ${starterPidBytes})"
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

# update datum stuff

# update the register redeemer to put the stake key on chain
# variable=$(cat stake.hash); jq --arg variable "$variable" '.fields[0].fields[0].bytes=$variable' ../../scripts/data/redeemers/register-redeemer.json > ../../scripts/data/redeemers/register-redeemer-new.json
# mv ../../scripts/data/redeemers/register-redeemer-new.json ../../scripts/data/redeemers/register-redeemer.json

echo "DONE"