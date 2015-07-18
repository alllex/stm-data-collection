#!/bin/bash

ghc -o Bench BenchmarkPQ.hs ../Benchmark.hs gettime.c ../BenchData.hs -O2 -threaded -rtsopts -fno-omit-yields
if [ $? -eq 0 ]; then
  echo OK

  ./Bench timing 1000 100 +RTS -N4

  # TIMELIMIT=50
  # INITSIZE=2000
  # REPEATS=10
  #
  # for i in `seq 1 10`
  # do
  #   OPCOUNT=$(( $i * 2000 ))
  #   PERIOD=$(( $i * 25 ))
  #   ./Bench timing $OPCOUNT $TIMELIMIT -s $INITSIZE -r 50 -n $REPEATS # +RTS -K16777216 -N4
  #   ./Bench throughput $PERIOD         -s $INITSIZE -r 50 -n $REPEATS # +RTS -K16777216 -N4
  # done

  # rm Bench
  # rm *.o *.hi
  # rm Internal/*.o Internal/*.hi
else
  echo FAIL
fi
