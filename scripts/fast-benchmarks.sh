#!/usr/bin/env sh

BENCHMARKS=$(cat ./scripts/benchmarks | awk '{if ($1 == "fast") print $2}')

etlas run eta-bench -- ${BENCHMARKS} --way="-O2" --jmh="-wi 1 -i 5 -gc true -prof gc -f 3" --run --compiler="./scripts/eta.sh"
