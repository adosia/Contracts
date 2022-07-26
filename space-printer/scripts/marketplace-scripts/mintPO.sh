#!/bin/bash
set -e

export CARDANO_NODE_SOCKET_PATH=$(cat path_to_socket.sh)
cli=$(cat path_to_cli.sh)
script_path="../../marketplace-contract/marketplace_contract.plutus"
mint_path="../../invoice-minting/minting_contract.plutus"

script_address=$(${cli} address build --payment-script-file ${script_path} --testnet-magic 1097911063)
#
customer_address=$(cat wallets/customer/payment.addr)
customer_pkh=$(cardano-cli address key-hash --payment-verification-key-file wallets/customer/payment.vkey)
#
designer_address=$(cat wallets/designer/payment.addr)
#
policy_id=$(cat ../../invoice-minting/policy.id)
#
name=$(echo -n "PurchaseOrder_1" | xxd -ps)

start_policy_id="16af70780a170994e8e5e575f4401b1d89bddf7d1a11d6264e0b0c85"
token_name="tBigTokenName12"
token_hex=$(echo -n ${token_name} | xxd -ps)
asset="1 ${start_policy_id}.${token_hex}"
MINT_ASSET="1 ${policy_id}.${name}"

min_utxo=$(${cli} transaction calculate-min-required-utxo \
    --alonzo-era \
    --protocol-params-file tmp/protocol.json \
    --tx-out-datum-embed-file data/datum/token_sale_datum.json \
    --tx-out="${script_address} ${asset}" | tr -dc '0-9')
script_address_out="${script_address} + ${min_utxo} + ${asset}"

UTXO_VALUE=$(${cli} transaction calculate-min-required-utxo \
    --protocol-params-file tmp/protocol.json \
    --tx-out="${customer_address} ${MINT_ASSET}" | tr -dc '0-9')
customer_address_out="${customer_address} + ${UTXO_VALUE} + ${MINT_ASSET}"

designer_address_out="${designer_address} + 10000000"

echo "Script OUTPUT: "${script_address_out}
echo "Customer OUTPUT: "${customer_address_out}
echo "Designer OUTPUT: "${designer_address_out}

#
# exit
#
echo -e "\033[0;36m Gathering Buyer UTxO Information  \033[0m"
${cli} query utxo \
    --testnet-magic 1097911063 \
    --address ${customer_address} \
    --out-file tmp/customer_utxo.json

TXNS=$(jq length tmp/customer_utxo.json)
if [ "${TXNS}" -eq "0" ]; then
   echo -e "\n \033[0;31m NO UTxOs Found At ${customer_address} \033[0m \n";
   exit;
fi
alltxin=""
TXIN=$(jq -r --arg alltxin "" 'keys[] | . + $alltxin + " --tx-in"' tmp/customer_utxo.json)
CTXIN=$(jq -r --arg alltxin "" 'keys[] | . + $alltxin + " --tx-in-collateral"' tmp/customer_utxo.json)
collateral_tx_in=${CTXIN::-19}
buyer_tx_in=${TXIN::-8}

echo -e "\033[0;36m Gathering Script UTxO Information  \033[0m"
${cli} query utxo \
    --address ${script_address} \
    --testnet-magic 1097911063 \
    --out-file tmp/script_utxo.json

# transaction variables
TXNS=$(jq length tmp/script_utxo.json)
if [ "${TXNS}" -eq "0" ]; then
   echo -e "\n \033[0;31m NO UTxOs Found At ${script_address} \033[0m \n";
   exit;
fi
alltxin=""
TXIN=$(jq -r --arg alltxin "" 'keys[] | . + $alltxin + " --tx-in"' tmp/script_utxo.json)
script_tx_in=${TXIN::-8}

# exit
echo -e "\033[0;36m Building Tx \033[0m"
FEE=$(${cli} transaction build \
    --babbage-era \
    --protocol-params-file tmp/protocol.json \
    --out-file tmp/tx.draft \
    --change-address ${customer_address} \
    --tx-in-collateral ${collateral_tx_in} \
    --tx-in ${buyer_tx_in} \
    --tx-in ${script_tx_in} \
    --tx-in-script-file ${script_path} \
    --tx-in-datum-file data/datum/token_sale_datum.json \
    --tx-in-redeemer-file data/redeemer/mint_redeemer.json \
    --tx-out="${designer_address_out}" \
    --tx-out="${customer_address_out}" \
    --tx-out="${script_address_out}" \
    --tx-out-datum-embed-file data/datum/token_sale_datum2.json \
    --mint="${MINT_ASSET}" \
    --mint-redeemer-file data/datum/token_sale_datum.json \
    --mint-script-file ${mint_path} \
    --testnet-magic 1097911063)

IFS=':' read -ra VALUE <<< "${FEE}"
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