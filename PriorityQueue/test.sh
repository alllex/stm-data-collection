#!/bin/bash
# run from project root directory as ./PriorityQueue/test.sh

echo Compile non threaded version
ghc -o TestPQ PriorityQueue/Test.hs -O2
if [ $? -eq 0 ]; then
  echo OK

  ./TestPQ

  rm TestPQ
  find . -type f -name '*.o' -delete
  find . -type f -name '*.hi' -delete
else
  echo FAIL
fi

ghc -o TestPQThreaded PriorityQueue/Test.hs -O2 -threaded -rtsopts -fno-omit-yields
if [ $? -eq 0 ]; then
  echo OK

  ./TestPQThreaded

  rm TestPQThreaded
  find . -type f -name '*.o' -delete
  find . -type f -name '*.hi' -delete
else
  echo FAIL
fi
