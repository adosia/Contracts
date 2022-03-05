#!/usr/bin/bash
set -e

# Check if user gave args
if [ $# -eq 0 ]
  then
    echo "Provide A Payment Address."
    exit
fi

# Mainnet
# PREFIX="addr"
# NET="61"

# Testnet
PREFIX="addr_test"
NET="60"

./bech32 ${PREFIX} <<< ${NET}${1}