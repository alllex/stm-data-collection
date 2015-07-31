#!/bin/bash
# run from project root as `./benhmarks/bench.sh`

BENCH_NAME=$1

# common benchmark parameters
RUNS=10
INITSIZE=25000
INSRATE=50
NUMCPUS=(1 2 4)

# throughput parameters
TIMEOUT_START=50
TIMEOUT_STEP=150
TIMEOUT_UPTO=500

# timing parameters
TIMELIMIT=200
OPCOUNT_START=10000
OPCOUNT_STEP=15000
OPCOUNT_UPTO=100000

# prepare for benchmarking
cabal configure --enable-benchmarks && cabal build -j

if [ $? -ne 0 ]; then
  echo FAILURE on build
  exit
fi

# iterating through number of CPUs enabled for benchmark
for NUMCPU in "${NUMCPUS[@]}"
do

    # benchmark throughput
    cabal bench $BENCH_NAME --only \
                --benchmark-options="throughput $TIMEOUT_START  \
                                    --step=$TIMEOUT_STEP        \
                                    --upto=$TIMEOUT_UPTO        \
                                    --runs=$RUNS                \
                                    --initsize=$INITSIZE        \
                                    --insrate=$INSRATE          \
                                    +RTS -K16m -qa -N$NUMCPU"
    # benchmark timing
    cabal bench $BENCH_NAME --only \
                --benchmark-options="timing $TIMELIMIT $OPCOUNT_START   \
                                    --step=$OPCOUNT_STEP                \
                                    --upto=$OPCOUNT_UPTO                \
                                    --runs=$RUNS                        \
                                    --initsize=$INITSIZE                \
                                    --insrate=$INSRATE                  \
                                    +RTS -K16m -qa -N$NUMCPU"
done
