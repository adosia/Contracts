#!/bin/bash
set -e

# SET UP VARS HERE
export CARDANO_NODE_SOCKET_PATH=$(cat path_to_socket.sh)
cli=$(cat path_to_cli.sh)

${cli} transaction witness \
    --tx-body-file tmp/tx.draft \
    --signing-key-file policy/policy.skey \
    --out-file tmp/tx.witness \
    --testnet-magic 2

${cli} transaction assemble \
    --tx-body-file tmp/tx.draft \
    --witness-file tmp/tx.witness \
    --witness-file tmp/wallet.witness \
    --out-file tmp/tx.signed

${cli} transaction submit \
    --testnet-magic 2 \
    --tx-file tmp/tx.signed

# exit 


# echo -e "\033[0;36m Signing \033[0m"
# ${cli} transaction sign \
#     --signing-key-file policy/policy.skey \
#     --tx-file tmp/tx.signed \
#     --out-file tmp/tx.signed \
#     --testnet-magic 2

# echo -e "\033[0;36m Submitting \033[0m"
# ${cli} transaction submit \
#     --testnet-magic 2 \
#     --tx-file tmp/tx.signed