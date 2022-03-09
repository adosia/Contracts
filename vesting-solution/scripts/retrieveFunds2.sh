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
provider_address=$(cat wallets/provider-wallet/payment.addr)

# Token Information
policy_id="57fca08abbaddee36da742a839f7d83a7e1d2419f1507fcbf3916522"
token_name="CHOC"
amount=10000
token_hex=$(echo -n ${token_name} | xxd -ps)
vestor_asset="1000 ${policy_id}.${token_hex}"
sc_asset="8000 ${policy_id}.${token_hex}"

# minimum ada to get in
vestor_min_value=$(${cli} transaction calculate-min-required-utxo \
    --protocol-params-file tmp/protocol.json \
    --tx-out-datum-embed-file data/create_vestment_datum.json \
    --tx-out="${vestor_address} ${vestor_asset}" | tr -dc '0-9')
vestor_address_out="${vestor_address} + ${vestor_min_value} + ${vestor_asset}"
echo "Vestor OUTPUT: "${vestor_address_out}

sc_min_value=$(${cli} transaction calculate-min-required-utxo \
    --protocol-params-file tmp/protocol.json \
    --tx-out-datum-embed-file data/retrieved_vestment_datum.json \
    --tx-out="${script_address} ${sc_asset}" | tr -dc '0-9')
sc_address_out="${script_address} + ${sc_min_value} + ${sc_asset}"
echo "Script OUTPUT: "${sc_address_out}

# exit

echo -e "\033[0;36m Gathering UTxO Information  \033[0m"
${cli} query utxo \
    --testnet-magic 1097911063 \
    --address ${vestor_address} \
    --out-file tmp/vestor_utxo.json

TXNS=$(jq length tmp/vestor_utxo.json)
if [ "$TXNS" -eq "0" ]; then
   echo -e "\n \033[0;31m NO UTxOs Found At ${vestor_address} \033[0m \n";
   exit;
fi
alltxin=""
TXIN=$(jq -r --arg alltxin "" 'keys[] | . + $alltxin + " --tx-in"' tmp/vestor_utxo.json)
CTXIN=$(jq -r --arg alltxin "" 'keys[] | . + $alltxin + " --tx-in-collateral"' tmp/vestor_utxo.json)
collateral_tx_in=${CTXIN::-19}
vestor_tx_in=${TXIN::-8}

echo -e "\033[0;36m Gathering Script UTxO Information  \033[0m"
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
script_tx_in=${TXIN::-8}

currentSlot=$(${cli} query tip --testnet-magic 1097911063 | jq .slot)

echo -e "\033[0;36m Building Tx \033[0m"
FEE=$(${cli} transaction build \
    --alonzo-era \
    --protocol-params-file tmp/protocol.json \
    --invalid-before $currentSlot \
    --invalid-hereafter 99999999 \
    --out-file tmp/tx.draft \
    --change-address ${vestor_address} \
    --tx-in ${vestor_tx_in} \
    --tx-in-collateral c630404afc060fb2695c08881dde6f215a42afbff4aaaf1a23586738e8b32bec#0 \
    --tx-in ${script_tx_in}  \
    --tx-in-datum-file data/retrieved_vestment_datum.json \
    --tx-in-redeemer-file data/retrieve_redeemer.json \
    --tx-out="${vestor_address} + ${vestor_min_value} + 1000 ${policy_id}.${token_hex}" \
    --tx-out ${provider_address}+1000000 \
    --tx-out="${vestor_address_out}" \
    --tx-out="${sc_address_out}" \
    --tx-out-datum-embed-file data/retrieved_vestment_datum2.json \
    --required-signer wallets/vestor-wallet/payment.skey \
    --tx-in-script-file ${script_path} \
    --testnet-magic 1097911063)

    # --tx-in-collateral ${collateral_tx_in} \
IFS=':' read -ra VALUE <<< "$FEE"
IFS=' ' read -ra FEE <<< "${VALUE[1]}"
FEE=${FEE[1]}
echo -e "\033[1;32m Fee: \033[0m" $FEE
#
# exit
#
echo -e "\033[0;36m Signing \033[0m"
${cli} transaction sign \
    --signing-key-file wallets/vestor-wallet/payment.skey \
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