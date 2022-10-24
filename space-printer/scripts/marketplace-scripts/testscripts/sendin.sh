${cli} transaction build \
    --babbage-era \
    --protocol-params-file tmp/protocol.json \
    --out-file tmp/tx.draft \
    --change-address ${designer_address} \
    --tx-in ${designer_tx_in} \
    --tx-out="${script_address_out}" \
    --tx-out-inline-datum-file data/marketplace_datum.json  \
    --mint-script-file policy/policy.script \
    --mint="${mint_asset}" \
    --metadata-json-file ${metadata_file} \
    --required-signer-hash ${designer_pkh} \
    --required-signer-hash ${minter_pkh} \
    ${network}