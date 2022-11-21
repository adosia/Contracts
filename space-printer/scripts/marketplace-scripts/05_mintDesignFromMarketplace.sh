#!/bin/bash
set -e

export CARDANO_NODE_SOCKET_PATH=$(cat path_to_socket.sh)
cli=$(cat path_to_cli.sh)
testnet_magic=2

script_path="../../marketplace-contract/marketplace-contract.plutus"
pool_path="../../printing-pool/printing-pool.plutus"
mint_path="../../invoice-minting/minting-contract.plutus"

# get current params
${cli} query protocol-parameters --testnet-magic ${testnet_magic} --out-file ./tmp/protocol.json

# Addresses
script_address=$(${cli} address build --payment-script-file ${script_path} --testnet-magic ${testnet_magic})
pool_address=$(${cli} address build --payment-script-file ${pool_path} --testnet-magic ${testnet_magic})

# collat
collat_address=$(cat wallets/collat/payment.addr)
collat_pkh=$(${cli} address key-hash --payment-verification-key-file wallets/collat/payment.vkey)

#
customer_address=$(cat wallets/customer/payment.addr)
customer_pkh=$(cardano-cli address key-hash --payment-verification-key-file wallets/customer/payment.vkey)

#
designer_address=$(cat wallets/designer/payment.addr)

# design info
starterPid=$(cat ../design-minter-scripts/data/datum/token_design_datum.json | jq -r .fields[0].bytes)
starterTkn=$(cat data/datum/token_sale_datum.json | jq -r .fields[2].bytes)
asset="1 ${starterPid}.${starterTkn}"

# purchase order info
poNum=$(cat data/datum/token_sale_datum.json | jq -r .fields[3].int)
poPid=$(cat data/datum/token_sale_datum.json | jq -r .fields[4].bytes)
poTkn=$(cat data/datum/token_sale_datum.json | jq -r .fields[2].bytes)$(echo -n "_" | od -A n -t x1 | sed 's/ *//g' | tr -d '\n')$(echo -n ${poNum} | od -A n -t x1 | sed 's/ *//g' | tr -d '\n')
mint_asset="1 ${poPid}.${poTkn}"


# update the printing pool datum
variable=${poTkn}; jq --arg variable "$variable" '.fields[0].fields[3].bytes=$variable' ../printing-pool-scripts/data/datum/printing_pool_datum.json > ../printing-pool-scripts/data/datum/printing_pool_datum-new.json
mv ../printing-pool-scripts/data/datum/printing_pool_datum-new.json ../printing-pool-scripts/data/datum/printing_pool_datum.json

# purchase order price
poPrice=$(cat data/datum/token_sale_datum.json | jq -r .fields[5].int)

nextPoNum=$((${poNum} + 1))

cp ./data/datum/token_sale_datum.json ./data/datum/updated_token_sale_datum.json

variable=${nextPoNum}; jq --argjson variable "$variable" '.fields[3].int=$variable' ./data/datum/updated_token_sale_datum.json > ./data/datum/updated_token_sale_datum-new.json
mv ./data/datum/updated_token_sale_datum-new.json ./data/datum/updated_token_sale_datum.json

current_min_utxo=$(${cli} transaction calculate-min-required-utxo \
    --babbage-era \
    --protocol-params-file ./tmp/protocol.json \
    --tx-out-inline-datum-file ./data/datum/token_sale_datum.json \
    --tx-out="${script_address} + 5000000 + ${asset}" | tr -dc '0-9')

updated_min_utxo=$(${cli} transaction calculate-min-required-utxo \
    --babbage-era \
    --protocol-params-file ./tmp/protocol.json \
    --tx-out-inline-datum-file ./data/datum/updated_token_sale_datum.json \
    --tx-out="${script_address} + 5000000 + ${asset}" | tr -dc '0-9')

difference=$((${updated_min_utxo} - ${current_min_utxo}))

if [ "$difference" -le "0" ]; then
    min_utxo=${current_min_utxo}

    # update the increase ada in the redeemer
    variable=0; jq --argjson variable "$variable" '.fields[0].fields[0].int=$variable' ./data/redeemer/mint_redeemer.json > ./data/redeemer/mint_redeemer-new.json
    mv ./data/redeemer/mint_redeemer-new.json ./data/redeemer/mint_redeemer.json
else
    echo "Increase Min ADA by" ${difference}
    min_utxo=${updated_min_utxo}

    # update the increase ada in the redeemer
    variable=${difference}; jq --argjson variable "$variable" '.fields[0].fields[0].int=$variable' ./data/redeemer/mint_redeemer.json > ./data/redeemer/mint_redeemer-new.json
    mv ./data/redeemer/mint_redeemer-new.json ./data/redeemer/mint_redeemer.json
fi

mv ./data/datum/updated_token_sale_datum.json ./data/datum/token_sale_datum.json

mint_min_utxo=$(${cli} transaction calculate-min-required-utxo \
    --babbage-era \
    --protocol-params-file tmp/protocol.json \
    --tx-out-inline-datum-file ../printing-pool-scripts/data/datum/printing_pool_datum.json \
    --tx-out="${customer_address} + 5000000 + ${mint_asset}" | tr -dc '0-9')

design_to_be_returned="${script_address} + ${min_utxo} + ${asset}"
customer_address_out="${pool_address} + ${mint_min_utxo} + ${mint_asset}"
designer_address_out="${designer_address} + ${poPrice}"

echo "Script OUTPUT: "${design_to_be_returned}
echo "Customer OUTPUT: "${customer_address_out}
echo "Designer OUTPUT: "${designer_address_out}
#
# exit
#
echo -e "\033[0;36m Gathering Customer UTxO Information  \033[0m"
${cli} query utxo \
    --address ${customer_address} \
    --testnet-magic ${testnet_magic} \
    --out-file tmp/customer_utxo.json

TXNS=$(jq length tmp/customer_utxo.json)
if [ "${TXNS}" -eq "0" ]; then
   echo -e "\n \033[0;31m NO UTxOs Found At ${customer_address} \033[0m \n";
   exit;
fi
alltxin=""
TXIN=$(jq -r --arg alltxin "" 'keys[] | . + $alltxin + " --tx-in"' tmp/customer_utxo.json)
customer_tx_in=${TXIN::-8}

echo -e "\033[0;36m Gathering Script UTxO Information  \033[0m"
${cli} query utxo \
    --address ${script_address} \
    --testnet-magic ${testnet_magic} \
    --out-file tmp/script_utxo.json

# transaction variables
TXNS=$(jq length tmp/script_utxo.json)
if [ "${TXNS}" -eq "0" ]; then
   echo -e "\n \033[0;31m NO UTxOs Found At ${script_address} \033[0m \n";
   exit;
fi
alltxin=""
TXIN=$(jq -r --arg alltxin "" --arg policy_id "$starterPid" --arg name "$starterTkn" 'to_entries[] | select(.value.value[$policy_id][$name] == 1) | .key | . + $alltxin + " --tx-in"' tmp/script_utxo.json)
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

script_ref_utxo=$(${cli} transaction txid --tx-file ../reference-txs/tx-marketplace-reference.signed)

# exit
echo -e "\033[0;36m Building Tx \033[0m"
FEE=$(${cli} transaction build \
    --babbage-era \
    --protocol-params-file tmp/protocol.json \
    --out-file tmp/tx.draft \
    --change-address ${customer_address} \
    --tx-in-collateral ${collat_utxo} \
    --tx-in ${customer_tx_in} \
    --tx-in ${script_tx_in} \
    --spending-tx-in-reference="${script_ref_utxo}#1" \
    --spending-plutus-script-v2 \
    --spending-reference-tx-in-inline-datum-present \
    --spending-reference-tx-in-redeemer-file ./data/redeemer/mint_redeemer.json \
    --tx-out="${designer_address_out}" \
    --tx-out="${customer_address_out}" \
    --tx-out-inline-datum-file ../printing-pool-scripts/data/datum/printing_pool_datum.json \
    --tx-out="${design_to_be_returned}" \
    --tx-out-inline-datum-file ./data/datum/token_sale_datum.json \
    --required-signer-hash ${collat_pkh} \
    --mint="${mint_asset}" \
    --mint-tx-in-reference="${script_ref_utxo}#2" \
    --mint-plutus-script-v2 \
    --policy-id="${poPid}" \
    --mint-reference-tx-in-redeemer-file ./data/datum/token_sale_datum.json \
    --metadata-json-file metadata.json \
    --testnet-magic ${testnet_magic})

IFS=':' read -ra VALUE <<< "${FEE}"
IFS=' ' read -ra FEE <<< "${VALUE[1]}"
FEE=${FEE[1]}
echo -e "\033[1;32m Fee: \033[0m" $FEE
#
exit
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