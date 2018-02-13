#!/usr/bin/env sh

BENCHMARKS=""
for i in $(cat slow-benchmarks); do
    BENCHMARKS="$BENCHMARKS $i"
done
eta-bench $BENCHMARKS --way="-O2" --jmh="-wi 0 -i 1" --run
