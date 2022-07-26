#!/usr/bin/bash
set -e
# SET UP VARS HERE
export CARDANO_NODE_SOCKET_PATH=$(cat path_to_socket.sh)
cli=$(cat path_to_cli.sh)

script_path="../../marketplace-contract/marketplace_contract.plutus"

script=$(${cli} address build --payment-script-file ${script_path} --testnet-magic 1097911063)

designer=$(cat wallets/designer/payment.addr)
customer=$(cat wallets/customer/payment.addr)
printer=$(cat wallets/printer/payment.addr)

echo
${cli} query protocol-parameters --testnet-magic 1097911063 --out-file tmp/protocol.json
${cli} query tip --testnet-magic 1097911063 | jq

echo
echo "Script Address:" ${script}
${cli} query utxo --address ${script} --testnet-magic 1097911063

echo
echo "Designer Address:" ${designer}
${cli} query utxo --address ${designer} --testnet-magic 1097911063

echo
echo "Customer Address:" ${customer}
${cli} query utxo --address ${customer} --testnet-magic 1097911063

echo
echo "Printer Address:" ${printer}
${cli} query utxo --address ${printer} --testnet-magic 1097911063

