#!/bin/bash
set -e

# SET UP VARS HERE
export CARDANO_NODE_SOCKET_PATH=$(cat path_to_socket.sh)
cli=$(cat path_to_cli.sh)
testnet_magic=2
script_path="../../marketplace-contract/marketplace-contract.plutus"

# get current params
${cli} query protocol-parameters --testnet-magic ${testnet_magic} --out-file ./tmp/protocol.json

# Addresses
script_address=$(${cli} address build --payment-script-file ${script_path} --testnet-magic ${testnet_magic})
echo -e "Script:" $script_address

# designer
designer_address=$(cat wallets/designer/payment.addr)
designer_pkh=$(${cli} address key-hash --payment-verification-key-file wallets/designer/payment.vkey)
echo -e "\nDesigner:" ${designer_address}

# collat
collat_address=$(cat wallets/collat/payment.addr)
collat_pkh=$(${cli} address key-hash --payment-verification-key-file wallets/collat/payment.vkey)

# asset to trade
starterPid=$(cat ../../start_info.json | jq -r .starterPid)
starterTkn=$(cat ./data/datum/token_sale_datum.json  | jq -r .fields[2].bytes)

asset="1 ${starterPid}.${starterTkn}"

cp ./data/datum/token_sale_datum.json ./data/datum/updated_token_sale_datum.json

if [[ $# -eq 0 ]] ; then
    echo -e "\n \033[0;31m Please Supply A New Design Price In Lovelace \033[0m \n";
    exit
fi

if [[ ${1} -lt 1000000 ]] ; then
    echo -e "\n \033[0;31m Design Price Must Be More Than 1000000 Lovelace \033[0m \n";
    exit
fi

echo -e "New Design Price Is ${1} Lovelace\n" 

# # update the register redeemer to put the stake key on chain
variable=${1}; jq --argjson variable "$variable" '.fields[6].int=$variable' ./data/datum/updated_token_sale_datum.json > ./data/datum/updated_token_sale_datum-new.json
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
    variable=0; jq --argjson variable "$variable" '.fields[0].fields[0].int=$variable' ./data/redeemer/update_redeemer.json > ./data/redeemer/update_redeemer-new.json
    mv ./data/redeemer/update_redeemer-new.json ./data/redeemer/update_redeemer.json
else
    echo "Increase Min ADA by" ${difference}
    min_utxo=${updated_min_utxo}

    # update the increase ada in the redeemer
    variable=${difference}; jq --argjson variable "$variable" '.fields[0].fields[0].int=$variable' ./data/redeemer/update_redeemer.json > ./data/redeemer/update_redeemer-new.json
    mv ./data/redeemer/update_redeemer-new.json ./data/redeemer/update_redeemer.json
fi

mv ./data/datum/updated_token_sale_datum.json ./data/datum/token_sale_datum.json

design_to_be_updated="${script_address} + ${min_utxo} + ${asset}"
echo -e "\nCreating A New Token Sale In The Marketplace:\n" ${design_to_be_updated}
#
# exit
#
echo -e "\033[0;36m Getting Designer UTxO Information  \033[0m"
${cli} query utxo \
    --testnet-magic ${testnet_magic} \
    --address ${designer_address} \
    --out-file tmp/designer_utxo.json

TXNS=$(jq length tmp/designer_utxo.json)
if [ "$TXNS" -eq "0" ]; then
   echo -e "\n \033[0;31m NO UTxOs Found At ${designer_address} \033[0m \n";
   exit;
fi
alltxin=""
TXIN=$(jq -r --arg alltxin "" 'keys[] | . + $alltxin + " --tx-in"' tmp/designer_utxo.json)
designer_tx_in=${TXIN::-8}

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

script_ref_utxo=$(${cli} transaction txid --tx-file tmp/tx-marketplace-reference.signed )

echo -e "\033[0;36m Building Tx \033[0m"
FEE=$(${cli} transaction build \
    --babbage-era \
    --protocol-params-file tmp/protocol.json \
    --out-file tmp/tx.draft \
    --change-address ${designer_address} \
    --tx-in-collateral="${collat_utxo}" \
    --tx-in ${designer_tx_in} \
    --tx-in ${script_tx_in}  \
    --spending-tx-in-reference="${script_ref_utxo}#1" \
    --spending-plutus-script-v2 \
    --spending-reference-tx-in-inline-datum-present \
    --spending-reference-tx-in-redeemer-file ./data/redeemer/update_redeemer.json \
    --tx-out="${design_to_be_updated}" \
    --tx-out-inline-datum-file ./data/datum/token_sale_datum.json \
    --required-signer-hash ${designer_pkh} \
    --required-signer-hash ${collat_pkh} \
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
    --signing-key-file wallets/designer/payment.skey \
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