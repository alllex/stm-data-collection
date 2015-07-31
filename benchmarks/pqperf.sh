#!/bin/bash

CPUS=$1

echo Configuring and building package
cabal configure --enable-benchmarks && cabal build -j &> /dev/null
if [ $? -ne 0 ]; then
    echo FAILURE on build
    exit $?
fi

PERF_DIR=perfdata
PERF_DATA=perf.data
PERF_PROG=dist/build/pqueue-bench/pqueue-bench
echo Start perf
sudo perf stat -a \
            -e cache-misses \
            -e L1-dcache-load-misses \
            -e L1-dcache-store-misses \
            -e L1-dcache-prefetch-misses \
            -e L1-icache-load-misses \
    ./$PERF_PROG throughput 400 \
                            --step=200 \
                            --upto=800              \
                            --runs=10               \
                            --initsize=10000        \
                            --insrate=50            \
                            +RTS -N$CPUS
