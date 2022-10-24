#!/bin/bash
set -e

# SET UP VARS HERE
export CARDANO_NODE_SOCKET_PATH=$(cat path_to_socket.sh)
cli=$(cat path_to_cli.sh)
script_path="../../marketplace-contract/marketplace-contract.plutus"

# Addresses
script_address=$(${cli} address build --payment-script-file ${script_path} --testnet-magic 2)
echo -e "Script: " $script_address

designer_address=$(cat wallets/designer/payment.addr)
echo -e "\nDesigner:" ${designer_address}

# Define Asset to be printed here
policy_id="16af70780a170994e8e5e575f4401b1d89bddf7d1a11d6264e0b0c85"
token_name="tBigTokenName12"
token_hex=$(echo -n ${token_name} | xxd -ps)
asset="1 ${policy_id}.${token_hex}"

min_utxo=$(${cli} transaction calculate-min-required-utxo \
    --babbage-era \
    --protocol-params-file tmp/protocol.json \
    --tx-out-datum-embed-file data/datum/token_sale_datum.json \
    --tx-out="${script_address} ${asset}" | tr -dc '0-9')
token_to_be_sold="${script_address} + ${min_utxo} + ${asset}"
echo -e "\nCreating A New Token Sale In The Marketplace:\n" ${token_to_be_sold}
#
exit
#
echo
echo -e "\033[0;36m Getting Customer UTxO Information  \033[0m"
${cli} query utxo \
    --testnet-magic 2 \
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
    --tx-out="${token_to_be_sold}" \
    --tx-out-datum-embed-file data/datum/token_sale_datum.json \
    --testnet-magic 2)

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
    --tx-body-file tmp/tx.draft \
    --out-file tmp/tx.signed \
    --testnet-magic 2
#
# exit
#
echo -e "\033[0;36m Submitting \033[0m"
${cli} transaction submit \
    --testnet-magic 2 \
    --tx-file tmp/tx.signed