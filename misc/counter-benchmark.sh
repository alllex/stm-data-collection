#!/bin/bash

ghc counter-benchmark.hs -O2 
if [ $? -eq 0 ]; then
  echo OK
  ./counter-benchmark -r 10 -t 20 
else
  echo FAIL
fi
