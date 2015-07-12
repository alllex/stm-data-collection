#!/bin/bash

ghc -o benchmark Benchmark.hs gettime.c -O2 -threaded -rtsopts -fno-omit-yields
if [ $? -eq 0 ]; then
  echo OK
  TIMELIMIT=100
  INITSIZE=2000
  REPEATS=10
  
  for i in `seq 1 10`
  do
    OPCOUNT=$(( $i * 2000 ))
    PERIOD=$(( $i * 50 ))
    ./benchmark timing $OPCOUNT $TIMELIMIT -s $INITSIZE -r 50 -n $REPEATS +RTS -K16777216 -N4 
    ./benchmark throughput $PERIOD         -s $INITSIZE -r 50 -n $REPEATS +RTS -K16777216 -N4 
  done
else
  echo FAIL
fi
