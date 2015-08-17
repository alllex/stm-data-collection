#!/bin/bash

./benchmarks/capbench.sh pq-bench
./benchmarks/capbench.sh bag-bench

cd charts
pdflatex capcharts.tex
rm capcharts.log
cd ..
