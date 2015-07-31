#!/bin/bash

function timestamp {
    echo "$(date +%Y%m%d.%H%M%S)"
}

cabal configure --enable-benchmarks && cabal build -j 2> /dev/null
if [ $? -ne 0 ]; then
    echo FAILURE on build
    exit
fi

PERF_DIR=perfdata
PERF_DATA=perf.data
PERF_PROG=./dist/build/pqueue-bench/pqueue-bench
perf record -a $PERF_PROG 
if [ ! `ls | grep $PERF_DATA` ]; then
    echo Something went wrong...
    exit
fi

if [ ! -d "$PERF_DIR" ]; then
    mkdir $PERF_DIR
fi
mv $PERF_DATA perfdata/pqperf.$(timestamp).data
