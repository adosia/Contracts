#!/bin/bash
set -e

export CARDANO_NODE_SOCKET_PATH=$(cat path_to_socket.sh)
cli=$(cat path_to_cli.sh)
network="--testnet-magic 2"

marketplace_script_path="../../marketplace-contract/marketplace-contract.plutus"
invoice_script_path="../../invoice-minting/minting-contract.plutus"
printing_pool_script_path="../../printing-pool/printing-pool.plutus"

# Addresses
reference_address=$(cat wallets/reference/payment.addr)

lock_min_utxo=$(${cli} transaction calculate-min-required-utxo \
    --babbage-era \
    --protocol-params-file tmp/protocol.json \
    --tx-out-reference-script-file ${marketplace_script_path} \
    --tx-out="${reference_address} + 5000000" | tr -dc '0-9')
echo "Marketplace Min Fee" ${lock_min_utxo}

mint_min_utxo=$(${cli} transaction calculate-min-required-utxo \
    --babbage-era \
    --protocol-params-file tmp/protocol.json \
    --tx-out-reference-script-file ${invoice_script_path} \
    --tx-out="${reference_address} + 5000000" | tr -dc '0-9')
echo "Invoice Min Fee" ${mint_min_utxo}

pool_min_utxo=$(${cli} transaction calculate-min-required-utxo \
    --babbage-era \
    --protocol-params-file tmp/protocol.json \
    --tx-out-reference-script-file ${printing_pool_script_path} \
    --tx-out="${reference_address} + 5000000" | tr -dc '0-9')
echo "Printing Pool Min Fee" ${pool_min_utxo}

echo
nft_lock_value=$lock_min_utxo
nft_mint_value=$mint_min_utxo
nft_pool_value=$pool_min_utxo
nft_lock_script_reference_utxo="${reference_address} + ${nft_lock_value}"
nft_mint_script_reference_utxo="${reference_address} + ${nft_mint_value}"
nft_pool_script_reference_utxo="${reference_address} + ${nft_pool_value}"

echo -e "\nCreating Marketplace Reference:\n" ${nft_lock_script_reference_utxo}
echo -e "\nCreating Invoice Reference:\n" ${nft_mint_script_reference_utxo}
echo -e "\nCreating Printing Pool Reference:\n" ${nft_pool_script_reference_utxo}
#
# exit
#
echo -e "\033[0;36m Gathering UTxO Information  \033[0m"
${cli} query utxo \
    ${network} \
    --address ${reference_address} \
    --out-file tmp/reference_utxo.json

TXNS=$(jq length tmp/reference_utxo.json)
if [ "${TXNS}" -eq "0" ]; then
   echo -e "\n \033[0;31m NO UTxOs Found At ${reference_address} \033[0m \n";
   exit;
fi
alltxin=""
TXIN=$(jq -r --arg alltxin "" 'keys[] | . + $alltxin + " --tx-in"' tmp/reference_utxo.json)
HEXTXIN=${TXIN::-8}

# chain second set of reference scripts to the first
echo -e "\033[0;36m Building Tx \033[0m"

starting_reference_lovelace=$(jq '[.. | objects | .lovelace] | add' tmp/reference_utxo.json)

${cli} transaction build-raw \
    --babbage-era \
    --protocol-params-file tmp/protocol.json \
    --out-file tmp/tx.draft \
    --tx-in ${HEXTXIN} \
    --tx-out="${reference_address} + ${starting_reference_lovelace}" \
    --tx-out="${nft_lock_script_reference_utxo}" \
    --tx-out-reference-script-file ${marketplace_script_path} \
    --tx-out="${nft_mint_script_reference_utxo}" \
    --tx-out-reference-script-file ${invoice_script_path} \
    --tx-out="${nft_pool_script_reference_utxo}" \
    --tx-out-reference-script-file ${printing_pool_script_path} \
    --fee 900000

FEE=$(${cli} transaction calculate-min-fee --tx-body-file tmp/tx.draft ${network} --protocol-params-file tmp/protocol.json --tx-in-count 0 --tx-out-count 0 --witness-count 1)
echo FEE: $FEE
fee=$(echo $FEE | rev | cut -c 9- | rev)

firstReturn=$((${starting_reference_lovelace} - ${nft_mint_value} - ${nft_lock_value} - ${fee}))

${cli} transaction build-raw \
    --babbage-era \
    --protocol-params-file tmp/protocol.json \
    --out-file tmp/tx.draft \
    --tx-in ${HEXTXIN} \
    --tx-out="${reference_address} + ${firstReturn}" \
    --tx-out="${nft_lock_script_reference_utxo}" \
    --tx-out-reference-script-file ${marketplace_script_path} \
    --tx-out="${nft_mint_script_reference_utxo}" \
    --tx-out-reference-script-file ${invoice_script_path} \
    --fee ${fee}

echo -e "\033[0;36m Signing \033[0m"
${cli} transaction sign \
    --signing-key-file wallets/reference/payment.skey \
    --tx-body-file tmp/tx.draft \
    --out-file tmp/tx-1.signed \
    ${network}


nextUTxO=$(${cli} transaction txid --tx-body-file tmp/tx.draft)
echo "First in the tx chain" $nextUTxO

${cli} transaction build-raw \
    --babbage-era \
    --protocol-params-file tmp/protocol.json \
    --out-file tmp/tx.draft \
    --tx-in="${nextUTxO}#0" \
    --tx-out="${reference_address} + ${firstReturn}" \
    --tx-out="${nft_pool_script_reference_utxo}" \
    --tx-out-reference-script-file ${printing_pool_script_path} \
    --fee 900000

FEE=$(${cli} transaction calculate-min-fee --tx-body-file tmp/tx.draft ${network} --protocol-params-file tmp/protocol.json --tx-in-count 0 --tx-out-count 0 --witness-count 1)
echo FEE: $FEE
fee=$(echo $FEE | rev | cut -c 9- | rev)

secondReturn=$((${firstReturn} - ${nft_pool_value} - ${fee}))

${cli} transaction build-raw \
    --babbage-era \
    --protocol-params-file tmp/protocol.json \
    --out-file tmp/tx.draft \
    --tx-in="${nextUTxO}#0" \
    --tx-out="${reference_address} + ${secondReturn}" \
    --tx-out="${nft_pool_script_reference_utxo}" \
    --tx-out-reference-script-file ${printing_pool_script_path} \
    --fee ${fee}

echo -e "\033[0;36m Signing \033[0m"
${cli} transaction sign \
    --signing-key-file wallets/reference/payment.skey \
    --tx-body-file tmp/tx.draft \
    --out-file tmp/tx-2.signed \
    ${network}

echo -e "\033[0;36m Submitting \033[0m"
${cli} transaction submit \
    ${network} \
    --tx-file tmp/tx-1.signed

echo -e "\033[0;36m Submitting \033[0m"
${cli} transaction submit \
    ${network} \
    --tx-file tmp/tx-2.signed

cp tmp/tx-1.signed tmp/tx-marketplace-reference.signed
cp tmp/tx-2.signed tmp/tx-printing-reference.signed