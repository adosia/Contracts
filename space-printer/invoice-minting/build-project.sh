cabal build -w ghc-8.10.7
cabal run invoice-minting

cardano-cli transaction policyid --script-file minting-contract.plutus > policy.id
python3 -c "import binascii;a='$(cat policy.id)';s=binascii.unhexlify(a);print([x for x in s])" > policy.bytes

echo -e "\nPolicy Id:" $(cat policy.id)
echo -e "\nPolicy Bytes:" $(cat policy.bytes)
echo "DONE"