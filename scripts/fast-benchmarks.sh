#!/usr/bin/env sh

BENCHMARKS=$(cat ./scripts/benchmarks | awk '{if ($1 == "fast") print $2}')

for b in ${BENCHMARKS}
do
  etlas run eta-bench -- ${b} --way="-O2" --jmh="-wi 1 -i 5 -gc true -prof gc -f 3" --run --compiler="./scripts/eta.sh" || exit
done
