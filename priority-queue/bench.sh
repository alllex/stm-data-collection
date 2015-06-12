#!/bin/bash

ghc Benchmark.hs gettime.c && echo OK && ./Benchmark
