#!/bin/bash

PCG_RANDOM_PATH=pkgs/pcg-random

cabal sandbox init --sandbox $PCG_RANDOM_PATH
cabal sandbox add-sources $PCG_RANDOM_PATH
