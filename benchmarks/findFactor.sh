#!/bin/bash

FACTORS=(1 2 4 8 16 32)
NUMCPUS=(1 2 4)
FILENAME="src/Data/STM/PriorityQueue/Internal/PTSTASLPQ.hs"
DEF_FACTOR=8

function changeFactor {
        echo "ARGUMENT: $1"
        sed -i -- "s/cacheFactor = [0-9]\+/cacheFactor = $1/g" $FILENAME
}

for NUMCPU in "${NUMCPUS[@]}"
do
    echo "CPUs($NUMCPU)"
    for FACTOR in "${FACTORS[@]}"
    do
        echo "FACTOR($FACTOR)"
        changeFactor $FACTOR
        ./benchmarks/pqperf.sh $NUMCPU | egrep 'Throughput|ms'
        if [ $? -ne 0 ]; then
            echo Something went wrong...
            changeFactor DEF_FACTOR
            exit $?
        fi
    done
done
changeFactor DEF_FACTOR
