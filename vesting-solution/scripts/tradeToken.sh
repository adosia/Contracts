#!/bin/bash
set -e

# SET UP VARS HERE
export CARDANO_NODE_SOCKET_PATH=$(cat path_to_socket.sh)
cli=$(cat path_to_cli.sh)
script_path="../token-sale/token_sale.plutus"

# Addresses
seller_address=$(cat seller-wallet/payment.addr)
buyer_address=$(cat buyer-wallet/payment.addr)
# signer_address="addr_test1vrxm0qpeek38dflguvrpp87hhewthd0mda44tnd45rjxqdg0vlz8d"
# signer_address=$(cat seller-wallet/payment.addr)
signer_address="addr_test1qq6gsyzzhtkaqcvpfwwdau525nxzlsl8ztsv9um5vqp749jtk39uar57zd2ythluu5eaqu3clq0alfeut7mdzfk78hfsg6sgqc"
# echo -e "\nCustomer: " ${seller_address}

# Define Asset to be printed here
# policy_id="d0277c2fbe48f6c708505f01403fb6d02c27c2ef17d741e18eb598ee"
# token_name="KRAKEN"
# token_hex=$(echo -n ${token_name} | xxd -ps)
# asset="1 ${policy_id}.${token_hex}"
# asset="100 48664e8d76f2b15606677bd117a3eac9929c378ac547ed295518dfd5.74426967546f6b656e4e616d653032"
asset="900 16af70780a170994e8e5e575f4401b1d89bddf7d1a11d6264e0b0c85.74426967546f6b656e4e616d653132"

min_utxo=$(${cli} transaction calculate-min-required-utxo \
    --protocol-params-file tmp/protocol.json \
    --tx-out="${seller_address} ${asset}" | tr -dc '0-9')
token_to_be_traded="${seller_address} + ${min_utxo} + ${asset}"

echo -e "\nTrading A Token:\n" ${token_to_be_traded}
echo -e "\033[0;36m Gathering UTxO Information  \033[0m"
${cli} query utxo \
    --testnet-magic 1097911063 \
    --address ${buyer_address} \
    --out-file tmp/buyer_utxo.json

TXNS=$(jq length tmp/buyer_utxo.json)
if [ "$TXNS" -eq "0" ]; then
   echo -e "\n \033[0;31m NO UTxOs Found At ${buyer_address} \033[0m \n";
   exit;
fi
alltxin=""
TXIN=$(jq -r --arg alltxin "" 'keys[] | . + $alltxin + " --tx-in"' tmp/buyer_utxo.json)
HEXTXIN=${TXIN::-8}

echo -e "\033[0;36m Building Tx \033[0m"
FEE=$(${cli} transaction build \
    --alonzo-era \
    --protocol-params-file tmp/protocol.json \
    --invalid-hereafter 99999999 \
    --out-file tmp/tx.draft \
    --change-address ${buyer_address} \
    --tx-in ${HEXTXIN} \
    --tx-out="${seller_address}+1500000" \
    --testnet-magic 1097911063)

    # --tx-out="${token_to_be_traded}" \
    # --tx-out=${buyer_address}+2000000+"" \
    # --tx-out addr_test1qzzluk64skxn86mhy73fz5truw36nk92swxv2drhp848s2ask5vpeuck56tjuhdqy46tly9vr24m9yunfcyphxg9zq7s4myaw8+5000000 \
IFS=':' read -ra VALUE <<< "$FEE"
IFS=' ' read -ra FEE <<< "${VALUE[1]}"
FEE=${FEE[1]}
echo -e "\033[1;32m Fee: \033[0m" $FEE
exit
echo -e "\033[0;36m Signing \033[0m"
${cli} transaction sign \
    --signing-key-file seller-wallet/payment.skey \
    --tx-body-file tmp/tx.draft \
    --out-file tmp/tx.signed \
    --testnet-magic 1097911063
# exit
echo -e "\033[0;36m Submitting \033[0m"
${cli} transaction submit \
    --testnet-magic 1097911063 \
    --tx-file tmp/tx.signed