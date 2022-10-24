cd marketplace-contract
cabal build -w ghc-8.10.7
cabal run marketplace-contract

cardano-cli address build --payment-script-file marketplace-contract.plutus --testnet-magic 2 --out-file validator.addr
cardano-cli transaction policyid --script-file marketplace-contract.plutus > validator.hash
python3 -c "import binascii;a='$(cat validator.hash)';s=binascii.unhexlify(a);print([x for x in s])" > validator.bytes

echo -e "\nValidator Testnet Address:" $(cat validator.addr)
echo -e "\nValidator Hash:" $(cat validator.hash)
echo -e "\nValidator Bytes:" $(cat validator.bytes)

cd ..
# adds in the locking hash into the script
python3 -c "from update_contracts import changeLockHash;changeLockHash('./invoice-minting/src/InvoiceMinting.hs', './invoice-minting/src/InvoiceMinting-new.hs', $(cat ./marketplace-contract/validator.bytes))"
mv ./invoice-minting/src/InvoiceMinting-new.hs ./invoice-minting/src/InvoiceMinting.hs

cd invoice-minting

cabal build -w ghc-8.10.7
cabal run invoice-minting

cardano-cli transaction policyid --script-file minting-contract.plutus > policy.id
python3 -c "import binascii;a='$(cat policy.id)';s=binascii.unhexlify(a);print([x for x in s])" > policy.bytes

echo -e "\nPolicy Id:" $(cat policy.id)
echo -e "\nPolicy Bytes:" $(cat policy.bytes)

echo "DONE"