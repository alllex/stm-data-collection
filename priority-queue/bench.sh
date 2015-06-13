#!/bin/bash

ghc Benchmark.hs gettime.c -O2 -threaded -rtsopts 
if [ $? -eq 0 ]; then
  echo OK
  ./Benchmark +RTS -K16777216 -N4 -s
else
  echo FAIL
fi
