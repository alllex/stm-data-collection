#!/bin/bash

ghc -o Bench ../*.hs ../gettime.c BenchmarkPQ.hs -O2 -threaded -rtsopts -fno-omit-yields -eventlog
if [ $? -eq 0 ]; then
  echo OK
  ./Bench timing 50000 200 --runs=20 --initsize=5000 --insrate=50 +RTS -s -N4 -la -K16m -qa
else
  echo FAIL
fi
