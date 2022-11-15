#!/bin/bash
set -e

# SET UP VARS HERE
export CARDANO_NODE_SOCKET_PATH=$(cat path_to_socket.sh)
cli=$(cat path_to_cli.sh)

# Addresses
script_path="../../printing-pool/printing_pool.plutus"
script_address=$(${cli} address build --payment-script-file ${script_path} --testnet-magic 1097911063)
echo -e "Script: " $script_address

printer_address=$(cat wallets/printer/payment.addr)
echo -e "\nPrinter: " ${printer_address}

customer_address=$(cat wallets/customer/payment.addr)
echo -e "Customer: " ${customer_address}

# Define Asset to be printed here
policy_id="088e1964087c9a0415439fa641184f882f422b74c0ea77995dd765bf"
token_hex="50757263686173654f726465725f31"
asset="1 ${policy_id}.${token_hex}"

min_utxo=$(${cli} transaction calculate-min-required-utxo \
    --alonzo-era \
    --protocol-params-file tmp/protocol.json \
    --tx-out-datum-embed-file data/datums/offer_information_datum.json \
    --tx-out="${customer_address} ${asset}" | tr -dc '0-9')
# Customer puts there offer in the utxo

offer_price=$(cat data/datums/offer_information_datum.json  | jq .fields[0].fields[6].int)

job_to_be_selected="${customer_address} + ${min_utxo} + ${asset}"
echo -e "\nSelecting A Printing Job:\n" ${job_to_be_selected}
payment_return="${printer_address} + ${offer_price}"
echo -e "\nComplete Payment:\n" ${payment_return}

#
# exit
#

echo -e "\033[0;36m Getting Printer UTxO Information  \033[0m"
${cli} query utxo \
    --testnet-magic 1097911063 \
    --address ${customer_address} \
    --out-file tmp/customer_utxo.json

TXNS=$(jq length tmp/customer_utxo.json)
if [ "$TXNS" -eq "0" ]; then
   echo -e "\n \033[0;31m NO UTxOs Found At ${customer_address} \033[0m \n";
   exit;
fi
alltxin=""
TXIN=$(jq -r --arg alltxin "" 'keys[] | . + $alltxin + " --tx-in"' tmp/customer_utxo.json)
CTXIN=$(jq -r --arg alltxin "" 'keys[] | . + $alltxin + " --tx-in-collateral"' tmp/customer_utxo.json)
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
    --babbage-era \
    --protocol-params-file tmp/protocol.json \
    --out-file tmp/tx.draft \
    --change-address ${customer_address} \
    --tx-in ${HEXTXIN} \
    --tx-in-collateral ${COLLAT} \
    --tx-in ${SCRIPT_TXIN} \
    --tx-in-script-file ${script_path} \
    --tx-in-datum-file data/datums/shipping_information_datum.json \
    --tx-in-redeemer-file data/redeemers/accept_redeemer.json \
    --tx-out="${job_to_be_selected}" \
    --tx-out="${payment_return}" \
    --required-signer wallets/customer/payment.skey \
    --testnet-magic 1097911063)

IFS=':' read -ra VALUE <<< "$FEE"
IFS=' ' read -ra FEE <<< "${VALUE[1]}"
FEE=${FEE[1]}
echo -e "\033[1;32m Fee: \033[0m" $FEE
#
# exit
#
echo -e "\033[0;36m Signing \033[0m"
${cli} transaction sign \
    --signing-key-file wallets/customer/payment.skey \
    --tx-body-file tmp/tx.draft \
    --out-file tmp/tx.signed \
    --testnet-magic 1097911063
#
# exit
#
echo -e "\033[0;36m Submitting \033[0m"
${cli} transaction submit \
    --testnet-magic 1097911063 \
    --tx-file tmp/tx.signed