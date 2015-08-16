#!/bin/bash
# run from project root as `./benhmarks/bench.sh`

BENCH_NAME=$1

# TMPFILE=__tmp.log

# common benchmark parameters
RUNS=15
INITSIZE=25000
INSRATE=50
NUMCPUS=(4)  # (1 2 4)

# throughput parameters
TIMEOUT_START=50
TIMEOUT_STEP=50
TIMEOUT_UPTO=750

# timing parameters
TIMELIMIT=500
OPCOUNT_START=10000
OPCOUNT_STEP=10000
OPCOUNT_UPTO=150000

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
                                    --file
                                    +RTS -K16m -qa -N$NUMCPU"
                                    # --scale=$NUMCPU             \

    # STAMP=$(cat $TMPFILE | head -n 1)
    # echo "------------------------------------------"
    # cat $TMPFILE
    # echo $STAMP >> mylog.log

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

    # echo "------------------------------------------"
    # cat $TMPFILE
    # echo "------------------------------------------"
    # STAMP=$(cat $TMPFILE | head -n 1)
    # echo $STAMP >> mylog.log

done

# rm $TMPFILE
