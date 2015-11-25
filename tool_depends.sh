#! /bin/sh
# Run me before you setup the sandbox.

cabal install 'happy == 1.19.*' --global

git submodule update --recursive
cd alex
cabal install quickcheck --global
runhaskell Setup.lhs configure
runhaskell Setup.lhs build
runhaskell Setup.lhs install
cd -
