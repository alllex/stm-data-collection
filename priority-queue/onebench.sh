#!/bin/bash

ghc -o Bench ../*.hs ../gettime.c BenchmarkPQ.hs -O2 -threaded -rtsopts -fno-omit-yields -eventlog
if [ $? -eq 0 ]; then
  echo OK
  # ./Bench timing 100000 200 --runs=50 --initsize=10000 --insrate=50 +RTS -s -N4 -la -K16m -qa
  ./Bench throughput 200 --runs=3 --initsize=10000 --insrate=50 +RTS -s -N4 -la -K16m -qa

  rm Bench
  rm *.o *.hi
  rm Internal/*.o Internal/*.hi
  rm ../*.o ../*.hi
else
  echo FAIL
fi
