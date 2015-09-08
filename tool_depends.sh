#! /bin/sh

cabal --require-sandbox install 'alex == 3.1.*'
cabal --require-sandbox install 'happy == 1.19.*'
