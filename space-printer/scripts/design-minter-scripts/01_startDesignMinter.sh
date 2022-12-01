#!/bin/bash
set -e

# SET UP VARS HERE
export CARDANO_NODE_SOCKET_PATH=$(cat path_to_socket.sh)
cli=$(cat path_to_cli.sh)
testnet_magic=$(cat ../testnet.magic)

# get params
${cli} query protocol-parameters --testnet-magic ${testnet_magic} --out-file tmp/protocol.json

# starting mary style policy
policy_path="policy/policy.script"
policy_pkh=$(${cli} address key-hash --payment-verification-key-file policy/policy.vkey)

# design lock path
script_path="../../design-locking-contract/locking-contract.plutus"
script_address=$(${cli} address build --payment-script-file ${script_path} --testnet-magic ${testnet_magic})

# collat, seller, reference
designer_address=$(cat wallets/designer/payment.addr)
designer_pkh=$(${cli} address key-hash --payment-verification-key-file wallets/designer/payment.vkey)

# design starter nft
policy_id=$(cat ../../start_info.json | jq -r .starterPid)
token_name=$(cat ../../start_info.json | jq -r .starterTkn)
asset="1 ${policy_id}.${token_name}"

# assume worst case
min_utxo=$(${cli} transaction calculate-min-required-utxo \
    --babbage-era \
    --protocol-params-file ./tmp/protocol.json \
    --tx-out-inline-datum-file ./data/datum/worst_case_datum.json \
    --tx-out="${script_address} + 5000000 + ${asset}" | tr -dc '0-9')

# reset the design counter to zero
variable=0; jq --argjson variable "$variable" '.fields[1].int=$variable' ./data/datum/token_design_datum.json > ./data/datum/token_design_datum-new.json
mv ./data/datum/token_design_datum-new.json ./data/datum/token_design_datum.json

# create the design prefix
designPrefix=$(echo -n "Adosia_Designs_" | od -A n -t x1 | sed 's/ *//g' | tr -d '\n')
variable=${designPrefix}; jq --arg variable "$variable" '.fields[2].bytes=$variable' ./data/datum/token_design_datum.json > ./data/datum/token_design_datum-new.json
mv ./data/datum/token_design_datum-new.json ./data/datum/token_design_datum.json

script_address_out="${script_address} + ${min_utxo} + ${asset}"
echo "Script OUTPUT: "${script_address_out}
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
HEXTXIN=${TXIN::-8}

echo -e "\033[0;36m Building Tx \033[0m"
FEE=$(${cli} transaction build \
    --babbage-era \
    --protocol-params-file tmp/protocol.json \
    --out-file tmp/tx.draft \
    --change-address ${designer_address} \
    --tx-in ${HEXTXIN} \
    --tx-out="${script_address_out}" \
    --tx-out-inline-datum-file ./data/datum/token_design_datum.json \
    --mint-script-file policy/policy.script \
    --mint="${asset}" \
    --required-signer-hash ${designer_pkh} \
    --required-signer-hash ${policy_pkh} \
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
    --signing-key-file policy/policy.skey \
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