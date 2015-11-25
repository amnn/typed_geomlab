#! /bin/sh
# Run me before you setup the sandbox.

cabal --require-sandbox install 'alex-3.1.5'
cabal --require-sandbox install 'happy == 1.19.*'
