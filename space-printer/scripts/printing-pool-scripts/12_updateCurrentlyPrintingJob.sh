#!/bin/bash
set -e

# SET UP VARS HERE
export CARDANO_NODE_SOCKET_PATH=$(cat path_to_socket.sh)
cli=$(cat path_to_cli.sh)
testnet_magic=2

# Addresses
script_path="../../printing-pool/printing-pool.plutus"
script_address=$(${cli} address build --payment-script-file ${script_path} --testnet-magic ${testnet_magic})
echo -e "Script: " $script_address

# collat
collat_address=$(cat wallets/collat/payment.addr)
collat_pkh=$(${cli} address key-hash --payment-verification-key-file wallets/collat/payment.vkey)

printer_address=$(cat wallets/printer/payment.addr)
printer_pkh=$(cardano-cli address key-hash --payment-verification-key-file wallets/printer/payment.vkey)
echo -e "\nPrinter: " ${printer_address}

customer_address=$(cat wallets/customer/payment.addr)
customer_pkh=$(cardano-cli address key-hash --payment-verification-key-file wallets/customer/payment.vkey)
echo -e "Customer: " ${customer_address}

# design info
poPid=$(cat ../marketplace-scripts/data/datum/token_sale_datum.json | jq -r .fields[4].bytes)
poTkn=$(cat data/datum/printing_pool_datum.json | jq -r .fields[0].fields[3].bytes)
asset="1 ${poPid}.${poTkn}"

min_utxo=$(${cli} transaction calculate-min-required-utxo \
    --babbage-era \
    --protocol-params-file tmp/protocol.json \
    --tx-out-inline-datum-file ./data/datum/printing_pool_datum.json \
    --tx-out="$script_address + 5000000 + $asset" | tr -dc '0-9')

offer_price=$(cat data/datum/offer_information_datum.json  | jq .fields[0].fields[6].int)

job_to_be_selected="${script_address} + ${min_utxo} + ${asset}"
payment_return="${customer_address} + ${offer_price}"
echo -e "\nReturning Payment:\n" ${payment_return}
echo -e "\nSelecting A Printing Job:\n" ${job_to_be_selected}
#
# exit
#
echo -e "\033[0;36m Getting Printer UTxO Information  \033[0m"
${cli} query utxo \
    --testnet-magic ${testnet_magic} \
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
    --testnet-magic ${testnet_magic} \
    --out-file tmp/script_utxo.json
# transaction variables
TXNS=$(jq length tmp/script_utxo.json)
if [ "$TXNS" -eq "0" ]; then
   echo -e "\n \033[0;31m NO UTxOs Found At ${script_address} \033[0m \n";
   exit;
fi
alltxin=""
TXIN=$(jq -r --arg alltxin "" --arg policy_id "$poPid" --arg name "$poTkn" 'to_entries[] | select(.value.value[$policy_id][$name] == 1) | .key | . + $alltxin + " --tx-in"' tmp/script_utxo.json)
script_tx_in=${TXIN::-8}

# collat info
echo -e "\033[0;36m Gathering Collateral UTxO Information  \033[0m"
${cli} query utxo \
    --testnet-magic ${testnet_magic} \
    --address ${collat_address} \
    --out-file tmp/collat_utxo.json

TXNS=$(jq length tmp/collat_utxo.json)
if [ "${TXNS}" -eq "0" ]; then
   echo -e "\n \033[0;31m NO UTxOs Found At ${collat_address} \033[0m \n";
   exit;
fi
collat_utxo=$(jq -r 'keys[0]' tmp/collat_utxo.json)

script_ref_utxo=$(${cli} transaction txid --tx-file ../marketplace-scripts/tmp/tx-printing-reference.signed)

currentSlot=$(${cli} query tip --testnet-magic 1097911063 | jq .slot)
finalSlot=$(($currentSlot + 500))

echo -e "\033[0;36m Building Tx \033[0m"
FEE=$(${cli} transaction build \
    --babbage-era \
    --protocol-params-file tmp/protocol.json \
    --invalid-before ${currentSlot} \
    --invalid-hereafter ${finalSlot} \
    --out-file tmp/tx.draft \
    --change-address ${customer_address} \
    --tx-in ${HEXTXIN} \
    --tx-in-collateral ${COLLAT} \
    --tx-in ${SCRIPT_TXIN}  \
    --tx-in-script-file ${script_path} \
    --tx-in-datum-file data/datums/offer_information_datum.json \
    --tx-in-redeemer-file data/redeemers/remove_redeemer.json \
    --tx-out="${payment_return}" \
    --tx-out="${job_to_be_selected}" \
    --tx-out-datum-embed-file data/datums/printing_pool_datum.json \
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