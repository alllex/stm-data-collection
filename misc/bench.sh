#!/bin/bash

ghc -fno-omit-yields counter-benchmark.hs 
if [ $? -eq 0 ]; then
  echo OK
  ./counter-benchmark 
else
  echo FAIL
fi
