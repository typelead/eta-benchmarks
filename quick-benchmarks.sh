#!/usr/bin/env sh

BENCHMARKS=$(cat benchmarks | awk '{if ($1 == "fast") print $2}')

etlas run eta-bench -- $BENCHMARKS --way="-O2" --jmh="-wi 0 -i 1 -gc true -prof gc" --run
