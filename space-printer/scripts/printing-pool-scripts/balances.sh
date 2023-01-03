#!/usr/bin/bash
set -e
# SET UP VARS HERE
export CARDANO_NODE_SOCKET_PATH=$(cat path_to_socket.sh)
cli=$(cat path_to_cli.sh)
testnet_magic=$(cat ../testnet.magic)

script_path="../../printing-pool/printing-pool.plutus"
script=$(${cli} address build --payment-script-file ${script_path} --testnet-magic ${testnet_magic})

designer=$(cat wallets/designer/payment.addr)
customer=$(cat wallets/customer/payment.addr)
printer=$(cat wallets/printer/payment.addr)
reference=$(cat wallets/reference/payment.addr)
collat=$(cat wallets/collat/payment.addr)

echo
${cli} query protocol-parameters --testnet-magic ${testnet_magic} --out-file tmp/protocol.json
${cli} query tip --testnet-magic ${testnet_magic} | jq

echo
echo "Script Address:" ${script}
${cli} query utxo --address ${script} --testnet-magic ${testnet_magic}

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

# ${cli} query utxo --tx-in e00c245387ec35ec75711a0828b7eae9d0da2b72ae23eedef84567c7757ed41d#1 --testnet-magic ${testnet_magic} --out-file po.utxo

# cat po.utxo | jq to_entries[]