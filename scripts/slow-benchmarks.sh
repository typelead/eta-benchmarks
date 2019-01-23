#!/usr/bin/env sh

BENCHMARKS=$(cat ./scripts/benchmarks | awk '{if ($1 == "slow") print $2}')

for b in ${BENCHMARKS}
do
  etlas run eta-bench -- ${b} --way="-O2" --jmh="-wi 0 -i 1 -bm ss" --run --compiler="./scripts/eta.sh" || exit
done
