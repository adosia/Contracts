#!/bin/bash
set -e

# SET UP VARS HERE
export CARDANO_NODE_SOCKET_PATH=$(cat path_to_socket.sh)
cli=$(cat path_to_cli.sh)
testnet_magic=2
script_path="../../marketplace-contract/marketplace-contract.plutus"

# get current params
${cli} query protocol-parameters --testnet-magic ${testnet_magic} --out-file ./tmp/protocol.json

# Addresses
script_address=$(${cli} address build --payment-script-file ${script_path} --testnet-magic ${testnet_magic})
echo -e "Script: " $script_address

designer_address=$(cat wallets/designer/payment.addr)
designer_pkh=$(${cli} address key-hash --payment-verification-key-file wallets/designer/payment.vkey)
policy_pkh=$(${cli} address key-hash --payment-verification-key-file policy/policy.vkey)
echo -e "\nDesigner:" ${designer_address}

echo -e "\033[1;35m \nEnter The Design Name:\n \033[0m" 

read designName

if [ ! "$designName" ];then
   echo -e "\n \033[0;31m Design Name Is Required \033[0m \n";
   exit
fi
echo
designName=$(echo ${designName} | head -c 12 | tr -d '\n')
echo "Design Name: ${designName}"

# little random bit added to the design name
randomBit=$(tr -dc A-Za-z0-9 </dev/urandom | head -c 12)

# asset to trade
starterPid=$(cat ../../start_info.json | jq -r .starterPid)

starterTkn=$(echo -n "${designName}${randomBit}" | od -A n -t x1 | sed 's/ *//g' | tr -d '\n')
designTkn=$(echo -n "${designName}${randomBit}_" | od -A n -t x1 | sed 's/ *//g' | tr -d '\n')

# exit

# update the register redeemer to put the stake key on chain
variable=${starterTkn}; jq --arg variable "$variable" '.fields[2].bytes=$variable' ./data/datum/token_sale_datum.json > ./data/datum/token_sale_datum-new.json
mv ./data/datum/token_sale_datum-new.json ./data/datum/token_sale_datum.json

# update the register redeemer to put the stake key on chain
variable=${designTkn}; jq --arg variable "$variable" '.fields[5].bytes=$variable' ./data/datum/token_sale_datum.json > ./data/datum/token_sale_datum-new.json
mv ./data/datum/token_sale_datum-new.json ./data/datum/token_sale_datum.json

variable=0; jq --argjson variable "$variable" '.fields[3].int=$variable' ./data/datum/token_sale_datum.json > ./data/datum/token_sale_datum-new.json
mv ./data/datum/token_sale_datum-new.json ./data/datum/token_sale_datum.json

variable=1000000; jq --argjson variable "$variable" '.fields[6].int=$variable' ./data/datum/token_sale_datum.json > ./data/datum/token_sale_datum-new.json
mv ./data/datum/token_sale_datum-new.json ./data/datum/token_sale_datum.json

asset="1 ${starterPid}.${starterTkn}"

min_utxo=$(${cli} transaction calculate-min-required-utxo \
    --babbage-era \
    --protocol-params-file ./tmp/protocol.json \
    --tx-out-inline-datum-file ./data/datum/token_sale_datum.json \
    --tx-out="${script_address} + 5000000 + ${asset}" | tr -dc '0-9')

design_to_be_sold="${script_address} + ${min_utxo} + ${asset}"
echo -e "\nCreating A New Token Sale In The Marketplace:\n" ${design_to_be_sold}
#
# exit
#
echo
echo -e "\033[0;36m Getting Customer UTxO Information  \033[0m"
${cli} query utxo \
    --testnet-magic ${testnet_magic} \
    --address ${designer_address} \
    --out-file tmp/designer_utxo.json

TXNS=$(jq length tmp/designer_utxo.json)
if [ "$TXNS" -eq "0" ]; then
   echo -e "\n \033[0;31m NO UTxOs Found At ${designer_address} \033[0m \n";
   exit;
fi
alltxin=""
TXIN=$(jq -r --arg alltxin "" 'keys[] | . + $alltxin + " --tx-in"' tmp/designer_utxo.json)
HEXTXIN=${TXIN::-8}

echo -e "\033[0;36m Building Tx \033[0m"
FEE=$(${cli} transaction build \
    --babbage-era \
    --protocol-params-file tmp/protocol.json \
    --out-file tmp/tx.draft \
    --change-address ${designer_address} \
    --tx-in ${HEXTXIN} \
    --tx-out="${design_to_be_sold}" \
    --tx-out-inline-datum-file ./data/datum/token_sale_datum.json \
    --mint-script-file policy/policy.script \
    --mint="${asset}" \
    --required-signer-hash ${designer_pkh} \
    --required-signer-hash ${policy_pkh} \
    --testnet-magic ${testnet_magic})

IFS=':' read -ra VALUE <<< "$FEE"
IFS=' ' read -ra FEE <<< "${VALUE[1]}"
FEE=${FEE[1]}
echo -e "\033[1;32m Fee: \033[0m" $FEE
#
# exit
#
echo -e "\033[0;36m Signing \033[0m"
${cli} transaction sign \
    --signing-key-file wallets/designer/payment.skey \
    --signing-key-file policy/policy.skey \
    --tx-body-file tmp/tx.draft \
    --out-file tmp/tx.signed \
    --testnet-magic ${testnet_magic}
#
# exit
#
echo -e "\033[0;36m Submitting \033[0m"
${cli} transaction submit \
    --testnet-magic ${testnet_magic} \
    --tx-file tmp/tx.signed