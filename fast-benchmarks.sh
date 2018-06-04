#!/usr/bin/env sh

BENCHMARKS=""
for i in $(cat fast-benchmarks); do
    BENCHMARKS="$BENCHMARKS $i"
done
etlas run eta-bench -- $BENCHMARKS --way="-O2" --jmh="-wi 1 -i 5 -gc true -prof gc -f 3" --run
