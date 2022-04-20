#!/bin/bash
set -e

# SET UP VARS HERE
export CARDANO_NODE_SOCKET_PATH=$(cat pathToSocket.sh)
cli=$(cat pathToCli.sh)
wallets="wallets"
script_path="../printing-pool/printing_pool.plutus"

# Addresses
script_address=$(${cli} address build --payment-script-file ${script_path} --testnet-magic 1097911063)
echo -e "Script: " $script_address

printer_address=$(cat wallets/printer/payment.addr)
customer_address=$(cat wallets/customer/payment.addr)
echo -e "\nPrinter: " ${printer_address}
echo -e "\Customer: " ${customer_address}

# Define Asset to be printed here
policy_id="16af70780a170994e8e5e575f4401b1d89bddf7d1a11d6264e0b0c85"
token_name="tBigTokenName12"
token_hex=$(echo -n ${token_name} | xxd -ps)
asset="1 ${policy_id}.${token_hex}"

min_utxo=$(${cli} transaction calculate-min-required-utxo \
    --alonzo-era \
    --protocol-params-file tmp/protocol.json \
    --tx-out-datum-embed-file data/datums/create_printing_pool_datum.json \
    --tx-out="$script_address $asset" | tr -dc '0-9')
offer_price=$(cat data/datums/create_printing_pool_datum.json  | jq .fields[0].fields[1].int)
offer_and_min=$((${min_utxo} + ${offer_price}))
job_to_be_selected="${script_address} + ${offer_and_min} + ${asset}"
printer_to_be_selected="${script_address} + 10000000"
echo -e "\nSelecting A Printing Job:\n" ${job_to_be_selected}
echo -e "\nSelecting A Printer:\n" ${printer_to_be_selected}
#
# exit
#
echo -e "\033[0;36m Getting Printer UTxO Information  \033[0m"
${cli} query utxo \
    --testnet-magic 1097911063 \
    --address ${printer_address} \
    --out-file tmp/printer_utxo.json

TXNS=$(jq length tmp/printer_utxo.json)
if [ "$TXNS" -eq "0" ]; then
   echo -e "\n \033[0;31m NO UTxOs Found At ${printer_address} \033[0m \n";
   exit;
fi
alltxin=""
TXIN=$(jq -r --arg alltxin "" 'keys[] | . + $alltxin + " --tx-in"' tmp/printer_utxo.json)
CTXIN=$(jq -r --arg alltxin "" 'keys[] | . + $alltxin + " --tx-in-collateral"' tmp/printer_utxo.json)
COLLAT=${CTXIN::-19}
HEXTXIN=${TXIN::-8}

echo -e "\033[0;36m Getting Script UTxO Information  \033[0m"
${cli} query utxo \
    --address ${script_address} \
    --testnet-magic 1097911063 \
    --out-file tmp/script_utxo.json
# transaction variables
TXNS=$(jq length tmp/script_utxo.json)
if [ "$TXNS" -eq "0" ]; then
   echo -e "\n \033[0;31m NO UTxOs Found At ${script_address} \033[0m \n";
   exit;
fi
alltxin=""
TXIN=$(jq -r --arg alltxin "" 'keys[] | . + $alltxin + " --tx-in"' tmp/script_utxo.json)
SCRIPT_TXIN=${TXIN::-8}


echo -e "\033[0;36m Building Tx \033[0m"
FEE=$(${cli} transaction build \
    --alonzo-era \
    --protocol-params-file tmp/protocol.json \
    --invalid-hereafter 99999999 \
    --out-file tmp/tx.draft \
    --change-address ${printer_address} \
    --tx-in ${HEXTXIN} \
    --tx-in-collateral ${COLLAT} \
    --tx-in 078a4f4d4a3c8122ab33536b51a872602e96e82d2c2bacb7f577301794354f4c#1 \
    --tx-in-datum-file data/datums/create_printing_pool_datum.json \
    --tx-in-redeemer-file data/redeemers/offer_redeemer.json \
    --tx-in-script-file ${script_path} \
    --tx-in afbe14ad4dd5cca9298787136cc9e3408a8c2674bdc2bc06ded4d00feb6b2797#1 \
    --tx-in-datum-file data/datums/create_printer_registration_datum.json \
    --tx-in-redeemer-file data/redeemers/prove_redeemer.json \
    --tx-in-script-file ${script_path} \
    --tx-out="${job_to_be_selected}" \
    --tx-out-datum-embed-file data/datums/make_offer_datum.json \
    --tx-out="${printer_to_be_selected}" \
    --tx-out-datum-embed-file data/datums/create_printer_registration_datum.json \
    --required-signer wallets/printer/payment.skey \
    --testnet-magic 1097911063)

IFS=':' read -ra VALUE <<< "$FEE"
IFS=' ' read -ra FEE <<< "${VALUE[1]}"
FEE=${FEE[1]}
echo -e "\033[1;32m Fee: \033[0m" $FEE
#
exit
#
echo -e "\033[0;36m Signing \033[0m"
${cli} transaction sign \
    --signing-key-file wallets/printer/payment.skey \
    --tx-body-file tmp/tx.draft \
    --out-file tmp/tx.signed \
    --testnet-magic 1097911063
#
exit
#
echo -e "\033[0;36m Submitting \033[0m"
${cli} transaction submit \
    --testnet-magic 1097911063 \
    --tx-file tmp/tx.signed