#!/bin/bash
set -e

export CARDANO_NODE_SOCKET_PATH=$(cat path_to_socket.sh)
cli=$(cat path_to_cli.sh)
testnet_magic=$(cat ../testnet.magic)

# get params
${cli} query protocol-parameters --testnet-magic ${testnet_magic} --out-file tmp/protocol.json

marketplace_script_path="../../marketplace-contract/marketplace-contract.plutus"
invoice_script_path="../../invoice-minting/minting-contract.plutus"
printing_pool_script_path="../../printing-pool/printing-pool.plutus"
design_lock_script_path="../../design-locking-contract/locking-contract.plutus"
design_mint_script_path="../../design-minting-contract/minting-contract.plutus"

# Addresses
reference_address=$(cat wallets/reference/payment.addr)

market_min_utxo=$(${cli} transaction calculate-min-required-utxo \
    --babbage-era \
    --protocol-params-file tmp/protocol.json \
    --tx-out-reference-script-file ${marketplace_script_path} \
    --tx-out="${reference_address} + 5000000" | tr -dc '0-9')

invoice_min_utxo=$(${cli} transaction calculate-min-required-utxo \
    --babbage-era \
    --protocol-params-file tmp/protocol.json \
    --tx-out-reference-script-file ${invoice_script_path} \
    --tx-out="${reference_address} + 5000000" | tr -dc '0-9')

pool_min_utxo=$(${cli} transaction calculate-min-required-utxo \
    --babbage-era \
    --protocol-params-file tmp/protocol.json \
    --tx-out-reference-script-file ${printing_pool_script_path} \
    --tx-out="${reference_address} + 5000000" | tr -dc '0-9')

design_lock_min_utxo=$(${cli} transaction calculate-min-required-utxo \
    --babbage-era \
    --protocol-params-file tmp/protocol.json \
    --tx-out-reference-script-file ${design_lock_script_path} \
    --tx-out="${reference_address} + 5000000" | tr -dc '0-9')

design_mint_min_utxo=$(${cli} transaction calculate-min-required-utxo \
    --babbage-era \
    --protocol-params-file tmp/protocol.json \
    --tx-out-reference-script-file ${design_mint_script_path} \
    --tx-out="${reference_address} + 5000000" | tr -dc '0-9')

echo
nft_market_value=${market_min_utxo}
nft_invoice_value=${invoice_min_utxo}
nft_pool_value=${pool_min_utxo}
nft_design_lock_value=${design_lock_min_utxo}
nft_design_mint_value=${design_mint_min_utxo}
#
nft_lock_script_reference_utxo="${reference_address} + ${nft_market_value}"
nft_mint_script_reference_utxo="${reference_address} + ${nft_invoice_value}"
nft_pool_script_reference_utxo="${reference_address} + ${nft_pool_value}"
nft_design_lock_script_reference_utxo="${reference_address} + ${nft_design_lock_value}"
nft_design_mint_script_reference_utxo="${reference_address} + ${nft_design_mint_value}"

echo -e "\nCreating Marketplace Reference:\n" ${nft_lock_script_reference_utxo}
echo -e "\nCreating Invoice Reference:\n" ${nft_mint_script_reference_utxo}
echo -e "\nCreating Printing Pool Reference:\n" ${nft_pool_script_reference_utxo}
echo -e "\nCreating Design Lock Reference:\n" ${nft_design_lock_script_reference_utxo}
echo -e "\nCreating Design Mint Reference:\n" ${nft_design_mint_script_reference_utxo}
#
# exit
#
echo -e "\033[0;36m Gathering UTxO Information  \033[0m"
${cli} query utxo \
    --testnet-magic ${testnet_magic} \
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
    --fee 900000

FEE=$(${cli} transaction calculate-min-fee --tx-body-file tmp/tx.draft --testnet-magic ${testnet_magic} --protocol-params-file tmp/protocol.json --tx-in-count 0 --tx-out-count 0 --witness-count 1)
echo FEE: $FEE
fee=$(echo $FEE | rev | cut -c 9- | rev)

firstReturn=$((${starting_reference_lovelace} - ${nft_invoice_value} - ${nft_market_value} - ${fee}))

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
    --testnet-magic ${testnet_magic}


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

FEE=$(${cli} transaction calculate-min-fee --tx-body-file tmp/tx.draft --testnet-magic ${testnet_magic} --protocol-params-file tmp/protocol.json --tx-in-count 0 --tx-out-count 0 --witness-count 1)
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
    --testnet-magic ${testnet_magic}

nextUTxO=$(${cli} transaction txid --tx-body-file tmp/tx.draft)
echo "First in the tx chain" $nextUTxO

${cli} transaction build-raw \
    --babbage-era \
    --protocol-params-file tmp/protocol.json \
    --out-file tmp/tx.draft \
    --tx-in="${nextUTxO}#0" \
    --tx-out="${reference_address} + ${secondReturn}" \
    --tx-out="${nft_design_lock_script_reference_utxo}" \
    --tx-out-reference-script-file ${design_lock_script_path} \
    --tx-out="${nft_design_mint_script_reference_utxo}" \
    --tx-out-reference-script-file ${design_mint_script_path} \
    --fee 900000

FEE=$(${cli} transaction calculate-min-fee --tx-body-file tmp/tx.draft --testnet-magic ${testnet_magic} --protocol-params-file tmp/protocol.json --tx-in-count 0 --tx-out-count 0 --witness-count 1)
echo FEE: $FEE
fee=$(echo $FEE | rev | cut -c 9- | rev)

thirdReturn=$((${secondReturn} - ${nft_design_lock_value} - ${nft_design_mint_value} - ${fee}))

${cli} transaction build-raw \
    --babbage-era \
    --protocol-params-file tmp/protocol.json \
    --out-file tmp/tx.draft \
    --tx-in="${nextUTxO}#0" \
    --tx-out="${reference_address} + ${thirdReturn}" \
    --tx-out="${nft_design_lock_script_reference_utxo}" \
    --tx-out-reference-script-file ${design_lock_script_path} \
    --tx-out="${nft_design_mint_script_reference_utxo}" \
    --tx-out-reference-script-file ${design_mint_script_path} \
    --fee ${fee}

echo -e "\033[0;36m Signing \033[0m"
${cli} transaction sign \
    --signing-key-file wallets/reference/payment.skey \
    --tx-body-file tmp/tx.draft \
    --out-file tmp/tx-3.signed \
    --testnet-magic ${testnet_magic}

echo -e "\033[0;36m Submitting \033[0m"
${cli} transaction submit \
    --testnet-magic ${testnet_magic} \
    --tx-file tmp/tx-1.signed

echo -e "\033[0;36m Submitting \033[0m"
${cli} transaction submit \
    --testnet-magic ${testnet_magic} \
    --tx-file tmp/tx-2.signed

echo -e "\033[0;36m Submitting \033[0m"
${cli} transaction submit \
    --testnet-magic ${testnet_magic} \
    --tx-file tmp/tx-3.signed

cp tmp/tx-1.signed ../reference-txs/tx-marketplace-reference.signed
cp tmp/tx-2.signed ../reference-txs/tx-printing-reference.signed
cp tmp/tx-3.signed ../reference-txs/tx-design-reference.signed