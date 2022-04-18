#!/bin/bash
set -e

# SET UP VARS HERE
export CARDANO_NODE_SOCKET_PATH=$(cat pathToSocket.sh)
cli=$(cat pathToCli.sh)

script_path="../printing-pool/printing_pool.plutus"

# Addresses
script_address=$(${cli} address build --payment-script-file ${script_path} --testnet-magic 1097911063)
echo -e "Script: " $script_address

printer_address=$(cat wallets/printer/payment.addr)
echo -e "\nCustomer: " ${printer_address}

printer_registration_out="${printer_address} + 10000000"
echo -e "\nRemoving A New Printing Job:\n" ${printer_registration_out}
#
# exit
#
echo -e "\033[0;36m Getting Customer UTxO Information  \033[0m"
${cli} query utxo \
    --testnet-magic 1097911063 \
    --address ${printer_address} \
    --out-file tmp/customer_utxo.json

TXNS=$(jq length tmp/customer_utxo.json)
if [ "$TXNS" -eq "0" ]; then
   echo -e "\n \033[0;31m NO UTxOs Found At ${printer_address} \033[0m \n";
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
    --alonzo-era \
    --protocol-params-file tmp/protocol.json \
    --invalid-hereafter 99999999 \
    --out-file tmp/tx.draft \
    --change-address ${printer_address} \
    --tx-in ${HEXTXIN} \
    --tx-in-collateral ${COLLAT} \
    --tx-in ${SCRIPT_TXIN}  \
    --tx-in-datum-file data/datums/create_printer_registration_datum.json \
    --tx-in-redeemer-file data/redeemers/remove_redeemer.json \
    --tx-out="${printer_registration_out}" \
    --required-signer wallets/printer/payment.skey \
    --tx-in-script-file ${script_path} \
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
    --signing-key-file wallets/printer/payment.skey \
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