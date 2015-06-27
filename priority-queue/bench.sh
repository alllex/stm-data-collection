#!/bin/bash

ghc -o benchmark Benchmark.hs gettime.c -O2 -threaded -rtsopts -fno-omit-yields
if [ $? -eq 0 ]; then
  echo OK
  TIMEOUT=1000
  ./benchmark timing 100   $TIMEOUT -s 10000 -r 100 +RTS -K16777216 -N4 
  ./benchmark timing 2500  $TIMEOUT -s 10000 -r 75  +RTS -K16777216 -N4 
  ./benchmark timing 5000  $TIMEOUT -s 10000 -r 50  +RTS -K16777216 -N4 
  ./benchmark timing 7500  $TIMEOUT -s 10000 -r 25  +RTS -K16777216 -N4 
  ./benchmark timing 10000 $TIMEOUT -s 10000 -r 1   +RTS -K16777216 -N4 

  ./benchmark throughput 1000 -s 0     -r 100 +RTS -K16777216 -N4 
  ./benchmark throughput 1000 -s 1000  -r 100 +RTS -K16777216 -N4 
  ./benchmark throughput 1000 -s 5000  -r 100 +RTS -K16777216 -N4 
  ./benchmark throughput 1000 -s 1000  -r 50 +RTS -K16777216 -N4 
  ./benchmark throughput 1000 -s 5000  -r 50 +RTS -K16777216 -N4 
  ./benchmark throughput 300  -s 5000  -r 25 +RTS -K16777216 -N4 
else
  echo FAIL
fi
