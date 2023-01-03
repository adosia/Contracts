#!/usr/bin/bash
set -e
# SET UP VARS HERE
export CARDANO_NODE_SOCKET_PATH=$(cat path_to_socket.sh)
cli=$(cat path_to_cli.sh)
testnet_magic=$(cat ../testnet.magic)

market_script_path="../../marketplace-contract/marketplace-contract.plutus"
design_script_path="../../design-locking-contract/locking-contract.plutus"

design=$(${cli} address build --payment-script-file ${design_script_path} --testnet-magic ${testnet_magic})
market=$(${cli} address build --payment-script-file ${market_script_path} --testnet-magic ${testnet_magic})

designer=$(cat wallets/designer/payment.addr)
customer=$(cat wallets/customer/payment.addr)
printer=$(cat wallets/printer/payment.addr)
reference=$(cat wallets/reference/payment.addr)
collat=$(cat wallets/collat/payment.addr)

echo
${cli} query protocol-parameters --testnet-magic ${testnet_magic} --out-file tmp/protocol.json
${cli} query tip --testnet-magic ${testnet_magic} | jq

echo
echo "Design Script Address:" ${design}
${cli} query utxo --address ${design} --testnet-magic ${testnet_magic}

echo
echo "Marketplace Script Address:" ${market}
${cli} query utxo --address ${market} --testnet-magic ${testnet_magic}


echo
echo "Designer Address:" ${designer}
${cli} query utxo --address ${designer} --testnet-magic ${testnet_magic}

echo
echo "Customer Address:" ${customer}
${cli} query utxo --address ${customer} --testnet-magic ${testnet_magic}

echo
echo "Printer Address:" ${printer}
${cli} query utxo --address ${printer} --testnet-magic ${testnet_magic}

echo
echo "Reference Address:" ${reference}
${cli} query utxo --address ${reference} --testnet-magic ${testnet_magic}

echo
echo "Collat Address:" ${collat}
${cli} query utxo --address ${collat} --testnet-magic ${testnet_magic}

# echo
# echo "Address:"
# ${cli} query utxo --address addr_test1qqk3u9geuu4zkl82chc5l5t3cwc6x38x7exqvel5ha3zspr6jeraypygwzs8ymmcvgvx8cphjlwp0w2xguarthk5ta6sszx960 --testnet-magic ${testnet_magic}

${cli} query utxo --address ${market} --testnet-magic ${testnet_magic} --out-file utxo.output
cat utxo.output | jq -r to_entries[].value.inlineDatum

# $(${cli} query utxo --address ${design} --testnet-magic ${testnet_magic} --out-file design.output) \
# cat design.output | jq