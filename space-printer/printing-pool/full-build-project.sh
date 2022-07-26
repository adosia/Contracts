cabal clean
cabal update
cabal build -w ghc-8.10.7
cabal run printing-pool
echo "DONE"