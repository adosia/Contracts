(${cli} transaction build \
    --babbage-era \
    --protocol-params-file tmp/protocol.json \
    --out-file tmp/tx.draft \
    --change-address ${designer_address} \
    --tx-in ${designer_tx_in} \
    --tx-in-collateral="${collat_utxo}" \
    --tx-in ${script_tx_in} \
    --spending-tx-in-reference="${script_ref_utxo}#1" \
    --spending-plutus-script-v2 \
    --spending-reference-tx-in-inline-datum-present \
    --spending-reference-tx-in-redeemer-file data/remove_redeemer.json \
    --tx-out="${designer_address_out}" \
    --required-signer-hash ${designer_pkh} \
    ${network}