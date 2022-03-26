#!/bin/bash
set -e

datum_hash=${1}
bfkey=$(cat blockfrost.api)
curl -H 'project_id: testnetH4w3Pty6JV590eTZ9kuJi9zJ2yFHUdJi' \
https://cardano-testnet.blockfrost.io/api/v0/scripts/datum/${datum_hash} | jq .json_value