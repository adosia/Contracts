#!/usr/bin/bash
set -e

export CARDANO_NODE_SOCKET_PATH=$(cat path_to_socket.sh)
cli=$(cat path_to_cli.sh)
script_path="../vesting-contract/vesting_contract.plutus"

script_address=$(${cli} address build --payment-script-file ${script_path} --testnet-magic 1097911063)
issuer_address=$(cat wallets/issuer-wallet/payment.addr)
vestor_address=$(cat wallets/vestor-wallet/payment.addr)
provider_address=$(cat wallets/provider-wallet/payment.addr)

echo
${cli} query tip --testnet-magic 1097911063 | jq

echo -e "\nScript Address:" ${script_address}
${cli} query utxo --address ${script_address} --testnet-magic 1097911063

echo -e "\nVestor Address:" ${vestor_address}
${cli} query utxo --address ${vestor_address} --testnet-magic 1097911063

echo -e "\nIssuer Address:" ${issuer_address}
${cli} query utxo --address ${issuer_address} --testnet-magic 1097911063

echo -e "\nProvider Address:" ${provider_address}
${cli} query utxo --address ${provider_address} --testnet-magic 1097911063