#!/bin/bash
set -e

# Paths
export CARDANO_NODE_SOCKET_PATH=$(cat path_to_socket.sh)
cli=$(cat path_to_cli.sh)
script_path="../vesting-contract/vesting_contract.plutus"

# Addresses
script_address=$(${cli} address build --payment-script-file ${script_path} --testnet-magic 1097911063)
issuer_address=$(cat wallets/issuer-wallet/payment.addr)
vestor_address=$(cat wallets/vestor-wallet/payment.addr)

# Token Information
policy_id="57fca08abbaddee36da742a839f7d83a7e1d2419f1507fcbf3916522"
token_name="CHOC"
amount=2000
token_hex=$(echo -n ${token_name} | xxd -ps)

# assets going in and out
sc_asset="${amount} ${policy_id}.${token_hex}"
change_asset="7001 ${policy_id}.${token_hex}"

# minimum ada to get in
min_value=$(${cli} transaction calculate-min-required-utxo \
    --protocol-params-file tmp/protocol.json \
    --tx-out-datum-embed-file data/create_vestment_datum.json \
    --tx-out="${script_address} ${sc_asset}" | tr -dc '0-9')
sc_address_out="${script_address} + ${min_value} + ${sc_asset}"
change_address_out="${issuer_address} + ${min_value} + ${change_asset}"
echo "Script OUTPUT: "${sc_address_out}
echo "Change OUTPUT: "${change_address_out}

# exit

echo -e "\033[0;36m Getting Buyer UTxO Information \033[0m"
# get utxo
${cli} query utxo \
    --testnet-magic 1097911063 \
    --address ${issuer_address} \
    --out-file tmp/issuer_utxo.json

# transaction variables
TXNS=$(jq length tmp/issuer_utxo.json)
alltxin=""
TXIN=$(jq -r --arg alltxin "" 'keys[] | . + $alltxin + " --tx-in"' tmp/issuer_utxo.json)
issuer_tx_in=${TXIN::-8}

echo -e "\033[0;36m Building Tx \033[0m"
# build tx
FEE=$(${cli} transaction build \
    --alonzo-era \
    --protocol-params-file tmp/protocol.json \
    --invalid-hereafter 99999999 \
    --out-file tmp/tx.draft \
    --change-address ${issuer_address} \
    --tx-in ${issuer_tx_in} \
    --tx-out="${change_address_out}" \
    --tx-out="${sc_address_out}" \
    --tx-out-datum-embed-file data/create_vestment_datum.json \
    --testnet-magic 1097911063)

IFS=':' read -ra VALUE <<< "$FEE"
IFS=' ' read -ra FEE <<< "${VALUE[1]}"
FEE=${FEE[1]}
echo -e "\033[1;32m Fee \033[0m" ${FEE}
#
# exit
#
echo -e "\033[0;36m Signing Tx \033[0m"
${cli} transaction sign \
    --signing-key-file wallets/issuer-wallet/payment.skey \
    --tx-body-file tmp/tx.draft \
    --out-file tmp/tx.signed \
    --testnet-magic 1097911063
#
# exit
#
echo -e "\033[0;36m Submitting Tx \033[0m"
${cli} transaction submit \
    --testnet-magic 1097911063 \
    --tx-file tmp/tx.signed