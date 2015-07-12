#!/bin/bash

ghc -o benchmark Benchmark.hs gettime.c -O2 -threaded -rtsopts -fno-omit-yields
if [ $? -eq 0 ]; then
  echo OK
  TIMELIMIT=1000
  INITSIZE=1000
  REPEATS=10
  
  for i in `seq 1 7`
  do
    OPCOUNT=$(( $i * 1000 ))
    PERIOD=$(( $i * 50 ))
    ./benchmark timing $OPCOUNT $TIMELIMIT -s $INITSIZE -r 50 -n $REPEATS +RTS -K16777216 -N4 
    ./benchmark throughput $PERIOD         -s $INITSIZE -r 50 -n $REPEATS +RTS -K16777216 -N4 
  done
  
  # ./benchmark throughput $PERIOD -s $INITSIZE -r 50 -n $REPEATS +RTS -K16777216 -N4 
else
  echo FAIL
fi
