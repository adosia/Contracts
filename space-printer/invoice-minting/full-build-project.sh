cabal clean
cabal update
cabal build -w ghc-8.10.7
cabal run invoice-minting
cardano-cli transaction policyid --script-file minting_contract.plutus > policy.id
echo "POLICY ID:" $(cat policy.id)
echo "DONE"