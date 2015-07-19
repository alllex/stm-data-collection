#!/bin/bash
# run from project root directory as ./PriorityQueue/pqbench.sh

ghc -o BenchPQ BenchmarkPQ.hs gettime.c -O2 -threaded -rtsopts -fno-omit-yields -eventlog
if [ $? -eq 0 ]; then
  echo OK
  TIMELIMIT=100
  OPCOUNT=10000
  STEP=10000
  UPTO=50000
  ./BenchPQ timing 100 10000 --step=10000 --upto=50000 --runs=10 --initsize=10000 --insrate=50 +RTS -s -N4 -la -K16m -qa
  # ./Bench throughput 200 --runs=3 --initsize=10000 --insrate=50 +RTS -s -N4 -la -K16m -qa

  rm BenchPQ
  find . -type f -name '*.o' -delete
  find . -type f -name '*.hi' -delete
else
  echo FAIL
fi
