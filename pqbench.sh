#!/bin/bash
# run from project root directory as ./PriorityQueue/pqbench.sh

ghc -o BenchPQ BenchmarkPQ.hs gettime.c \
    -O2 -threaded -rtsopts -fno-omit-yields -eventlog \
    -fPIC
if [ $? -eq 0 ]; then
  echo OK
  ./BenchPQ throughput 50 --step=20 --upto=250 \
            --runs=10 --initsize=10000 --insrate=50 \
            +RTS -s -N4 -la -K16m -qa

  # rm BenchPQ
  # find . -type f -name '*.o' -delete
  # find . -type f -name '*.hi' -delete
  # find . -type f -name '*.dyn_hi' -delete
  # find . -type f -name '*.dyn_o' -delete
else
  echo FAIL
fi
