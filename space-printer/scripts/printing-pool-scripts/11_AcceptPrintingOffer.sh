#!/bin/bash
set -e

split_string() {
    local string=$1
    local delimiter=$2
    local result=()

    while IFS=$delimiter read -ra line; do
        result+=("${line[@]}")
    done <<< "$string"

    echo "${result[@]}"
}

# SET UP VARS HERE
export CARDANO_NODE_SOCKET_PATH=$(cat path_to_socket.sh)
cli=$(cat path_to_cli.sh)
testnet_magic=$(cat ../testnet.magic)

# Addresses
script_path="../../printing-pool/printing-pool.plutus"
script_address=$(${cli} address build --payment-script-file ${script_path} --testnet-magic ${testnet_magic})
echo -e "Script: " $script_address

# collat
collat_address=$(cat wallets/collat/payment.addr)
collat_pkh=$(${cli} address key-hash --payment-verification-key-file wallets/collat/payment.vkey)

# printer
printer_address=$(cat wallets/printer/payment.addr)
printer_pkh=$(cardano-cli address key-hash --payment-verification-key-file wallets/printer/payment.vkey)
echo -e "Printer: " ${printer_address}

#customer
customer_address=$(cat wallets/customer/payment.addr)
customer_pkh=$(cardano-cli address key-hash --payment-verification-key-file wallets/customer/payment.vkey)
echo -e "Customer: " ${customer_address}

# design info
poPid=$(cat ../marketplace-scripts/data/datum/token_sale_datum.json | jq -r .fields[4].bytes)
poTkn=$(cat data/datum/printing_pool_datum.json | jq -r .fields[0].fields[3].bytes)
asset="1 ${poPid}.${poTkn}"

po_min_utxo=$(${cli} transaction calculate-min-required-utxo \
    --babbage-era \
    --protocol-params-file tmp/protocol.json \
    --tx-out-inline-datum-file ./data/datum/printing_pool_datum.json \
    --tx-out="$script_address + 5000000 + $asset" | tr -dc '0-9')

offer_min_utxo=$(${cli} transaction calculate-min-required-utxo \
    --babbage-era \
    --protocol-params-file tmp/protocol.json \
    --tx-out-inline-datum-file ./data/datum/make_offer_information_datum.json \
    --tx-out="$script_address + 5000000" | tr -dc '0-9')

cp ./data/datum/make_offer_information_datum.json ./data/datum/post_offer_information_datum.json

variable=2; jq --argjson variable "$variable" '.constructor=$variable' data/datum/post_offer_information_datum.json > data/datum/post_offer_information_datum-new.json
mv data/datum/post_offer_information_datum-new.json data/datum/post_offer_information_datum.json

offer_price=$(cat data/datum/make_offer_information_datum.json  | jq .fields[0].fields[6].int)

offer_and_min=$((${po_min_utxo} + ${offer_price}))
job_to_be_selected="${script_address} + ${offer_and_min} + ${asset}"
return_accepted_offer="${printer_address} + ${offer_min_utxo}"
echo -e "\nSelecting A Printing Job:\n" ${job_to_be_selected}
echo -e "\nReturning Accepted Offer:\n" ${return_accepted_offer}
#
# exit
#
echo -e "\033[0;36m Getting Customer UTxO Information  \033[0m"
${cli} query utxo \
    --testnet-magic ${testnet_magic} \
    --address ${customer_address} \
    --out-file tmp/customer_utxo.json

TXNS=$(jq length tmp/customer_utxo.json)
if [ "$TXNS" -eq "0" ]; then
   echo -e "\n \033[0;31m NO UTxOs Found At ${customer_address} \033[0m \n";
   exit;
fi
alltxin=""
TXIN=$(jq -r --arg alltxin "" 'keys[] | . + $alltxin + " --tx-in"' tmp/customer_utxo.json)
customer_tx_in=${TXIN::-8}

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
po_script_tx_in=${TXIN::-8}

alltxin=""
TXIN=$(jq -r --arg alltxin "" --arg printerPkh "${printer_pkh}" 'to_entries[] | select(.value.inlineDatum.fields[0].fields[4].bytes == $printerPkh) | .key | . + $alltxin + " --tx-in"' tmp/script_utxo.json)
offer_script_tx_in=${TXIN::-8}

string=$po_script_tx_in
IFS='#' read -ra array <<< "$string"
# update the offer price and tx id info
variable=${offer_price}; jq --argjson variable "$variable" '.fields[0].fields[0].int=$variable' ./data/redeemer/make_offer_redeemer.json > ./data/redeemer/make_offer_redeemer-new.json
mv ./data/redeemer/make_offer_redeemer-new.json ./data/redeemer/make_offer_redeemer.json
variable=${array[0]}; jq --arg variable "$variable" '.fields[0].fields[1].bytes=$variable' ./data/redeemer/make_offer_redeemer.json > ./data/redeemer/make_offer_redeemer-new.json
mv ./data/redeemer/make_offer_redeemer-new.json ./data/redeemer/make_offer_redeemer.json
variable=${array[1]}; jq --argjson variable "$variable" '.fields[0].fields[2].int=$variable' ./data/redeemer/make_offer_redeemer.json > ./data/redeemer/make_offer_redeemer-new.json
mv ./data/redeemer/make_offer_redeemer-new.json ./data/redeemer/make_offer_redeemer.json

string=$offer_script_tx_in
IFS='#' read -ra array <<< "$string"
# update the offer price and tx id info
variable=${offer_price}; jq --argjson variable "$variable" '.fields[0].fields[0].int=$variable' ./data/redeemer/accept_offer_redeemer.json > ./data/redeemer/accept_offer_redeemer-new.json
mv ./data/redeemer/accept_offer_redeemer-new.json ./data/redeemer/accept_offer_redeemer.json
variable=${array[0]}; jq --arg variable "$variable" '.fields[0].fields[1].bytes=$variable' ./data/redeemer/accept_offer_redeemer.json > ./data/redeemer/accept_offer_redeemer-new.json
mv ./data/redeemer/accept_offer_redeemer-new.json ./data/redeemer/accept_offer_redeemer.json
variable=${array[1]}; jq --argjson variable "$variable" '.fields[0].fields[2].int=$variable' ./data/redeemer/accept_offer_redeemer.json > ./data/redeemer/accept_offer_redeemer-new.json
mv ./data/redeemer/accept_offer_redeemer-new.json ./data/redeemer/accept_offer_redeemer.json

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

script_ref_utxo=$(${cli} transaction txid --tx-file ../reference-txs/tx-printing-reference.signed)

echo -e "\033[0;36m Building Tx \033[0m"
FEE=$(${cli} transaction build \
    --babbage-era \
    --protocol-params-file tmp/protocol.json \
    --out-file tmp/tx.draft \
    --change-address ${customer_address} \
    --tx-in ${customer_tx_in} \
    --tx-in-collateral ${collat_utxo} \
    --tx-in ${po_script_tx_in}  \
    --spending-tx-in-reference="${script_ref_utxo}#1" \
    --spending-plutus-script-v2 \
    --spending-reference-tx-in-inline-datum-present \
    --spending-reference-tx-in-redeemer-file ./data/redeemer/accept_offer_redeemer.json \
    --tx-in ${offer_script_tx_in}  \
    --spending-tx-in-reference="${script_ref_utxo}#1" \
    --spending-plutus-script-v2 \
    --spending-reference-tx-in-inline-datum-present \
    --spending-reference-tx-in-redeemer-file ./data/redeemer/make_offer_redeemer.json \
    --tx-out="${job_to_be_selected}" \
    --tx-out-inline-datum-file data/datum/post_offer_information_datum.json \
    --tx-out="${return_accepted_offer}" \
    --required-signer-hash ${customer_pkh} \
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
    --signing-key-file wallets/customer/payment.skey \
    --signing-key-file wallets/collat/payment.skey \
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