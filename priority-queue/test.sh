#!/bin/bash

echo Compile non threaded version
ghc -o Test Test.hs -O2 
if [ $? -eq 0 ]; then
  echo OK
  ./Test
else 
  echo FAIL
fi

echo Compile threaded version
ghc -o TestThreaded Test.hs -O2 -threaded -rtsopts -fno-omit-yields
if [ $? -eq 0 ]; then
  echo OK
  ./TestThreaded +RTS -K16777216 -N4 
else 
  echo FAIL
fi
