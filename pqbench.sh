#!/bin/bash
# run from project root directory as ./PriorityQueue/pqbench.sh

ghc -o BenchPQ BenchmarkPQ.hs gettime.c -O2 -threaded -rtsopts -fno-omit-yields -eventlog
if [ $? -eq 0 ]; then
  echo OK
  TIMELIMIT=200
  OPCOUNT=10000
  STEP=10000
  UPTO=100000
  ./BenchPQ timing $TIMELIMIT $OPCOUNT --step=$STEP --upto=$UPTO \
            --runs=10 --initsize=10000 --insrate=50 \
            +RTS -s -N4 -la -K16m -qa
  # ./Bench throughput 200 --runs=3 --initsize=10000 --insrate=50 +RTS -s -N4 -la -K16m -qa

  rm BenchPQ
  find . -type f -name '*.o' -delete
  find . -type f -name '*.hi' -delete
else
  echo FAIL
fi
