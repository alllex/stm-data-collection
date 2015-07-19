#!/bin/bash

ghc -o Bench BenchmarkPQ.hs ../Benchmark.hs gettime.c ../BenchData.hs -O2 -threaded -rtsopts -fno-omit-yields
if [ $? -eq 0 ]; then
  echo OK

  ./Bench timing 1000 100 +RTS -N4


  # rm Bench
  # rm *.o *.hi
  # rm Internal/*.o Internal/*.hi
else
  echo FAIL
fi
