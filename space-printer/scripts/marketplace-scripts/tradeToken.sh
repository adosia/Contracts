#!/bin/bash
set -e

# SET UP VARS HERE
export CARDANO_NODE_SOCKET_PATH=$(cat path_to_socket.sh)
cli=$(cat path_to_cli.sh)
testnet_magic=2

# Addresses
sender_wallet_path="wallets/printer/"
sender_address=$(cat ${sender_wallet_path}payment.addr)
sender_pkh=$(${cli} address key-hash --payment-verification-key-file ${sender_wallet_path}payment.vkey)

receiver_wallet_path="wallets/printer/"
receiver_address=$(cat ${receiver_wallet_path}payment.addr)
receiver_pkh=$(${cli} address key-hash --payment-verification-key-file ${receiver_wallet_path}payment.vkey)

# receiver_address="addr_test1qrupt9d9ug2ufnrrajp2q7gwvmrtzzgr80p5ug7q8nt4d66hu0s5mnhxh2853wtsgn9gdz6wuqtaqnkv0yk78p474d6qudapqh"

# Define Asset to be printed here
asset="1 f61e1c1d38fc4e5b0734329a4b7b820b76bb8e0729458c153c4248ea.5468697349734f6e6553746172746572546f6b656e466f7254657374696e6731"
CHANGE_ASSET=""

min_utxo=$(${cli} transaction calculate-min-required-utxo \
    --protocol-params-file tmp/protocol.json \
    --tx-out="${receiver_address}+ 5000000 + ${asset}" | tr -dc '0-9')

token_to_be_traded="${receiver_address} + ${min_utxo} + ${asset}"
change_return_out="${receiver_address} + 2500000"

echo -e "\nTrading A Token:\n" ${token_to_be_traded}
# echo -e "\nTrading A Token:\n" ${change_return_out}
#
# exit
#
echo -e "\033[0;36m Gathering UTxO Information  \033[0m"
${cli} query utxo \
    --testnet-magic ${testnet_magic} \
    --address ${sender_address} \
    --out-file tmp/sender_utxo.json

TXNS=$(jq length tmp/sender_utxo.json)
if [ "${TXNS}" -eq "0" ]; then
   echo -e "\n \033[0;31m NO UTxOs Found At ${sender_address} \033[0m \n";
   exit;
fi
alltxin=""
TXIN=$(jq -r --arg alltxin "" 'keys[] | . + $alltxin + " --tx-in"' tmp/sender_utxo.json)
HEXTXIN=${TXIN::-8}

echo -e "\033[0;36m Building Tx \033[0m"
FEE=$(${cli} transaction build \
    --babbage-era \
    --protocol-params-file tmp/protocol.json \
    --out-file tmp/tx.draft \
    --change-address ${sender_address} \
    --tx-in ${HEXTXIN} \
    --tx-out="${token_to_be_traded}" \
    --required-signer-hash ${sender_pkh} \
    --required-signer-hash ${receiver_pkh} \
    --testnet-magic ${testnet_magic})

IFS=':' read -ra VALUE <<< "${FEE}"
IFS=' ' read -ra FEE <<< "${VALUE[1]}"
FEE=${FEE[1]}
echo -e "\033[1;32m Fee: \033[0m" $FEE
#
# exit
#
${cli} transaction witness \
    --tx-body-file tmp/tx.draft \
    --signing-key-file ${sender_wallet_path}payment.skey \
    --out-file tmp/tx.witness \
    --testnet-magic 2

echo -e "\033[0;36m Sender Is Signing \033[0m"
${cli} transaction sign \
    --signing-key-file ${sender_wallet_path}payment.skey \
    --tx-body-file tmp/tx.draft \
    --out-file tmp/tx.signed \
    --testnet-magic ${testnet_magic}

echo -e "\033[0;36m Receiver Is Signing \033[0m"
${cli} transaction sign \
    --signing-key-file ${receiver_wallet_path}payment.skey \
    --tx-file tmp/tx.signed \
    --out-file tmp/tx.signed \
    --testnet-magic ${testnet_magic}
#
# exit
#
echo -e "\033[0;36m Submitting Tx \033[0m"
${cli} transaction submit \
    --testnet-magic ${testnet_magic} \
    --tx-file tmp/tx.signed