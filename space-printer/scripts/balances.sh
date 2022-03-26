#!/usr/bin/bash
set -e
# SET UP VARS HERE
export CARDANO_NODE_SOCKET_PATH=$(cat pathToSocket.sh)
cli=$(cat pathToCli.sh)

wallets="wallets"
script_path="../printing-pool/printing_pool.plutus"

script=$(${cli} address build --payment-script-file ${script_path} --testnet-magic 1097911063)
customer=$(cat ${wallets}/wallet-a/payment.addr)
printer=$(cat ${wallets}/wallet-b/payment.addr)


echo
echo "Script Address:" ${script}
${cli} query utxo --address ${script} --testnet-magic 1097911063

echo
echo "Customer Address:" ${customer}
${cli} query utxo --address ${customer} --testnet-magic 1097911063

echo
echo "Printer Address:" ${printer}
${cli} query utxo --address ${printer} --testnet-magic 1097911063

