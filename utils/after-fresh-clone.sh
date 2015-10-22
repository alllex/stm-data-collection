#!/bin/bash
# run from project root as `./utils/after-fresh-clone.sh`

echo --------------------------------------
echo Creating sandboxes
echo --------------------------------------
cabal sandbox init --sandbox . 

echo --------------------------------------
echo Installing dependencies
echo --------------------------------------
cabal install -j --only-dependencies --enable-tests --enable-benchmarks

echo --------------------------------------
echo Testing 
echo --------------------------------------
cabal configure && cabal test
