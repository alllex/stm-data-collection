#!/bin/bash
# run from project root as `./benhmarks/capbench.sh`

BENCH_NAME=$1

# common benchmark parameters
RUNS=10
INITSIZE=50000
INSRATE=50
NUMCPUS=(1 2 4)

# throughput parameters
PERIOD=500

# timing parameters
TIMELIMIT=500
OPCOUNT=100000

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
                --benchmark-options="throughput $PERIOD     \
                                    $SCALE_FLAG             \
                                    --runs=$RUNS            \
                                    --initsize=$INITSIZE    \
                                    --insrate=$INSRATE      \
                                    --file                  \
                                    +RTS -K16m -qa -N$NUMCPU"

    THR="$BENCH_NAME-period-s$INITSIZE-r$INSRATE-c$NUMCPU.log"
    THRTMP="$BENCH_NAME-period-s$INITSIZE-r$INSRATE.log"
    cat $THR | sed -r "s/^[0-9]+/$NUMCPU/g" >> $THRTMP

    # benchmark timing
    cabal bench $BENCH_NAME --only \
                --benchmark-options="timing $TIMELIMIT $OPCOUNT \
                                    --runs=$RUNS                \
                                    --initsize=$INITSIZE        \
                                    --insrate=$INSRATE          \
                                    --file                      \
                                    +RTS -K16m -qa -N$NUMCPU"

    TIM="$BENCH_NAME-timing-s$INITSIZE-r$INSRATE-c$NUMCPU.log"
    TIMTMP="$BENCH_NAME-timing-s$INITSIZE-r$INSRATE.log"
    cat $TIM | sed -r "s/^[0-9]+/$NUMCPU/g" >> $TIMTMP

    rm $THR
    rm $TIM
done

THRRES="charts/$BENCH_NAME-period$PERIOD-s$INITSIZE-r$INSRATE.log"
TIMRES="charts/$BENCH_NAME-timing$OPCOUNT-s$INITSIZE-r$INSRATE.log"

cat $THRTMP | head -n 1 > $THRRES
cat $THRTMP | awk 'NR % 2 == 0' >> $THRRES
sed -i 's/period/capabilities/' $THRRES

cat $TIMTMP | head -n 1 > $TIMRES
cat $TIMTMP | awk 'NR % 2 == 0' >> $TIMRES
sed -i 's/timing/capabilities/' $TIMRES

rm $THRTMP
rm $TIMTMP
