#!/bin/bash

# ghc -o Benchmark Benchmark.hs gettime.c -O2 -threaded -rtsopts -fno-omit-yields
ghc -o Benchmark Benchmark.hs gettime.c -O2 -rtsopts -fno-omit-yields
if [ $? -eq 0 ]; then
  echo OK
  TIMELIMIT=50
  INITSIZE=2000
  REPEATS=10
  
  for i in `seq 1 10`
  do
    OPCOUNT=$(( $i * 2000 ))
    PERIOD=$(( $i * 25 ))
    ./Benchmark timing $OPCOUNT $TIMELIMIT -s $INITSIZE -r 50 -n $REPEATS # +RTS -K16777216 -N4 
    ./Benchmark throughput $PERIOD         -s $INITSIZE -r 50 -n $REPEATS # +RTS -K16777216 -N4 
  done

  # rm Benchmark 
  # rm *.o *.hi
  # rm Internal/*.o Internal/*.hi
else
  echo FAIL
fi
