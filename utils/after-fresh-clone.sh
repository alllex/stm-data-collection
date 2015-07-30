#!/bin/bash
# run from project root as `./utils/after-fresh-clone.sh`

echo --------------------------------------
echo Updating git submodules
echo --------------------------------------
git submodule init && git submodule update

if [ $? -eq 0 ]; then
    echo --------------------------------------
    echo Creating sandboxes
    echo --------------------------------------
    PCG_RANDOM_PATH=pkgs/pcg-random
    cabal sandbox init --sandbox . 
    cabal sandbox add-source $PCG_RANDOM_PATH

    echo --------------------------------------
    echo Installing dependencies
    echo --------------------------------------
    cabal install -j --only-dependencies --enable-tests --enable-benchmarks
else
    echo FAILED to fetch submodules
fi
