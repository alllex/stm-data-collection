#!/bin/bash
# run from project root as `./benhmarks/pqbench.sh`

# common benchmark parameters
RUNS=10
INITSIZE=10000
INSRATE=50
NUMCPUS=(1 2 4)

# throughput parameters
TIMEOUT_START=50
TIMEOUT_STEP=50
TIMEOUT_UPTO=300

# timing parameters
TIMELIMIT=500
OPCOUNT_START=10000
OPCOUNT_STEP=15000
OPCOUNT_UPTO=100000

# prepare for benchmarking
cabal configure --enable-benchmarks && cabal build

if [ $? -eq 0 ]; then

    # iterating through number of CPUs enabled for benchmark
    for NUMCPU in "${NUMCPUS[@]}"
    do

        # benchmark throughput
        cabal bench --only \
                    --benchmark-options="throughput $TIMEOUT_START  \
                                        --step=$TIMEOUT_STEP        \
                                        --upto=$TIMEOUT_UPTO        \
                                        --runs=$RUNS                \
                                        --initsize=$INITSIZE        \
                                        --insrate=$INSRATE          \
                                        +RTS -s -la -K16m -qa -N$NUMCPU"
        # benchmark timing
        cabal bench --only \
                    --benchmark-options="timing $TIMELIMIT $OPCOUNT_START   \
                                        --step=$OPCOUNT_STEP                \
                                        --upto=$OPCOUNT_UPTO                \
                                        --runs=$RUNS                        \
                                        --initsize=$INITSIZE                \
                                        --insrate=$INSRATE                  \
                                        +RTS -s -la -K16m -qa -N$NUMCPU"
    done
else
  echo FAILURE on build
fi
