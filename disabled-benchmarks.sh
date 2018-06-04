#!/usr/bin/env sh

for i in $(cat disabled-benchmarks); do
    etlas run eta-bench -- $i --way="-O2" --jmh="-wi 0 -i 1" --run
done
