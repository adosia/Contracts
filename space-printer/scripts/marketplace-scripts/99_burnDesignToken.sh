#!/bin/bash
set -e

# SET UP VARS HERE
export CARDANO_NODE_SOCKET_PATH=$(cat path_to_socket.sh)
cli=$(cat path_to_cli.sh)
testnet_magic=$(cat ../testnet.magic)

design_policy_id=$(cat ../../design-minting-contract/policy.id)

design_lock_script_path="../../design-locking-contract/locking-contract.plutus"
design_mint_script_path="../../marketplace-contract/marketplace-contract.plutus"

# get current params
${cli} query protocol-parameters --testnet-magic ${testnet_magic} --out-file ./tmp/protocol.json

# Addresses
collat_address=$(cat wallets/collat/payment.addr)
collat_pkh=$(${cli} address key-hash --payment-verification-key-file wallets/collat/payment.vkey)

designer_address=$(cat wallets/designer/payment.addr)
designer_pkh=$(${cli} address key-hash --payment-verification-key-file wallets/designer/payment.vkey)

design_lock_address=$(${cli} address build --payment-script-file ${design_lock_script_path} --testnet-magic ${testnet_magic})

# design starter nft
policy_id=$(cat ../../start_info.json | jq -r .starterPid)
token_name=$(cat ../../start_info.json | jq -r .starterTkn)
starter_asset="1 ${policy_id}.${token_name}"

min_utxo=$(${cli} transaction calculate-min-required-utxo \
    --babbage-era \
    --protocol-params-file ./tmp/protocol.json \
    --tx-out-inline-datum-file ../design-minter-scripts/data/datum/worst_case_datum.json \
    --tx-out="${design_lock_address} + 5000000 + ${starter_asset}" | tr -dc '0-9')

returning_starter_nft="${design_lock_address} + ${min_utxo} + ${starter_asset}"

# asset to trade
starterPid=$(cat ../design-minter-scripts/data/datum/token_design_datum.json | jq -r .fields[0].bytes)
starterTkn=$(cat ./data/datum/token_sale_datum.json  | jq -r .fields[2].bytes)
asset="-1 ${starterPid}.${starterTkn}"

echo "Burning" ${asset}
#
# exit
#
echo
echo -e "\033[0;36m Getting Customer UTxO Information  \033[0m"
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
HEXTXIN=${TXIN::-8}

# design locking script
echo -e "\033[0;36m Gathering Script UTxO Information  \033[0m"
${cli} query utxo \
    --address ${design_lock_address} \
    --testnet-magic ${testnet_magic} \
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

script_ref_utxo=$(${cli} transaction txid --tx-file ../reference-txs/tx-design-reference.signed)


echo -e "\033[0;36m Building Tx \033[0m"
FEE=$(${cli} transaction build \
    --babbage-era \
    --protocol-params-file tmp/protocol.json \
    --out-file tmp/tx.draft \
    --change-address ${designer_address} \
    --tx-in-collateral="${collat_utxo}" \
    --tx-in ${HEXTXIN} \
    --tx-in ${script_tx_in} \
    --spending-tx-in-reference="${script_ref_utxo}#1" \
    --spending-plutus-script-v2 \
    --spending-reference-tx-in-inline-datum-present \
    --spending-reference-tx-in-redeemer-file ../design-minter-scripts/data/redeemer/burn_redeemer.json \
    --tx-out="${returning_starter_nft}" \
    --tx-out-inline-datum-file ../design-minter-scripts/data/datum/token_design_datum.json \
    --mint="${asset}" \
    --mint-tx-in-reference="${script_ref_utxo}#2" \
    --mint-plutus-script-v2 \
    --policy-id="${design_policy_id}" \
    --mint-reference-tx-in-redeemer-file ../design-minter-scripts/data/redeemer/mint_redeemer.json \
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