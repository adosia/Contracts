#!/bin/bash
set -e

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

printer_address=$(cat wallets/printer/payment.addr)
printer_pkh=$(cardano-cli address key-hash --payment-verification-key-file wallets/printer/payment.vkey)
echo -e "Printer: " ${printer_address}

# design info
poPid=$(cat ../marketplace-scripts/data/datum/token_sale_datum.json | jq -r .fields[4].bytes)
poTkn=$(cat data/datum/printing_pool_datum.json | jq -r .fields[0].fields[3].bytes)
asset="1 ${poPid}.${poTkn}"

min_utxo=$(${cli} transaction calculate-min-required-utxo \
    --babbage-era \
    --protocol-params-file tmp/protocol.json \
    --tx-out-inline-datum-file ./data/datum/make_offer_information_datum.json \
    --tx-out="$script_address + 5000000" | tr -dc '0-9')

# update the printing pool datum
variable=${poTkn}; jq --arg variable "$variable" '.fields[0].fields[3].bytes=$variable' data/datum/make_offer_information_datum.json > data/datum/make_offer_information_datum-new.json
mv data/datum/make_offer_information_datum-new.json data/datum/make_offer_information_datum.json

if [[ $# -eq 0 ]] ; then
    echo -e "\n \033[0;31m Please Supply A New Offer Price In Lovelace \033[0m \n";
    exit
fi

if [[ ${1} -lt 1000000 ]] ; then
    echo -e "\n \033[0;31m Offer Price Must Be More Than 1000000 Lovelace \033[0m \n";
    exit
fi

echo -e "\nNew Offer Price Is ${1} Lovelace\n" 

offer_price=${1}
prevRegionCode=$(cat data/datum/printing_pool_datum.json | jq -r .fields[0].fields[2].list[0].int)

# update region code
variable=${prevRegionCode}; jq --argjson variable "$variable" '.fields[0].fields[2].list[0].int=$variable' ./data/datum/make_offer_information_datum.json > ./data/datum/make_offer_information_datum-new.json
mv ./data/datum/make_offer_information_datum-new.json ./data/datum/make_offer_information_datum.json

# update offer price
variable=${offer_price}; jq --argjson variable "$variable" '.fields[0].fields[6].int=$variable' data/datum/make_offer_information_datum.json > data/datum/make_offer_information_datum-new.json
mv data/datum/make_offer_information_datum-new.json data/datum/make_offer_information_datum.json

offer_to_be_made="${script_address} + ${min_utxo}"
echo -e "\nSelecting A Printing Job:\n" ${offer_to_be_made}
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
printer_tx_in=${TXIN::-8}

echo -e "\033[0;36m Building Tx \033[0m"
FEE=$(${cli} transaction build \
    --babbage-era \
    --protocol-params-file tmp/protocol.json \
    --out-file tmp/tx.draft \
    --change-address ${printer_address} \
    --tx-in ${printer_tx_in} \
    --tx-out="${offer_to_be_made}" \
    --tx-out-inline-datum-file data/datum/make_offer_information_datum.json \
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
    --signing-key-file wallets/printer/payment.skey \
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