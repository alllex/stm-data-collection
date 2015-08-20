#!/bin/bash
# run from project root as `./benhmarks/bench.sh`

BENCH_NAME=$1

# common benchmark parameters
RUNS=5
INITSIZE=10000
INSRATE=50
NUMCPUS=(4)  # (1 2 4)

# throughput parameters
TIMEOUT_START=100
TIMEOUT_STEP=100
TIMEOUT_UPTO=300

# timing parameters
TIMELIMIT=500
OPCOUNT_START=20000
OPCOUNT_STEP=20000
OPCOUNT_UPTO=60000

# prepare for benchmarking
cabal configure --enable-benchmarks && cabal build -j

if [ $? -ne 0 ]; then
  echo FAILURE on build
  exit
fi

# iterating through number of CPUs enabled for benchmark
for NUMCPU in "${NUMCPUS[@]}"
do
    if [ "$BENCH_NAME" == "pq-bench" ]; then
       SCALE_FLAG="--scale=$NUMCPU"
    fi

    # benchmark throughput
    cabal bench $BENCH_NAME --only \
                --benchmark-options="throughput $TIMEOUT_START  \
                                    --step=$TIMEOUT_STEP        \
                                    --upto=$TIMEOUT_UPTO        \
                                    $SCALE_FLAG                 \
                                    --runs=$RUNS                \
                                    --initsize=$INITSIZE        \
                                    --insrate=$INSRATE          \
                                    --file
                                    +RTS -K16m -qa -N$NUMCPU"
                                    # --scale=$NUMCPU             \

    # benchmark timing
    cabal bench $BENCH_NAME --only \
                --benchmark-options="timing $TIMELIMIT $OPCOUNT_START   \
                                    --step=$OPCOUNT_STEP                \
                                    --upto=$OPCOUNT_UPTO                \
                                    --runs=$RUNS                        \
                                    --initsize=$INITSIZE                \
                                    --insrate=$INSRATE                  \
                                    --file
                                    +RTS -K16m -qa -N$NUMCPU"

done
