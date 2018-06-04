#!/usr/bin/env sh

BENCHMARKS=""
for i in $(cat slow-benchmarks); do
    BENCHMARKS="$BENCHMARKS $i"
done

etlas run eta-bench -- $BENCHMARKS --way="-O2" --jmh="-wi 0 -i 1 -bm ss" --run
