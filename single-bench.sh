#!/usr/bin/env sh

# ARGS: [test-name] [measurement iterations] [worker iterations]

eta-bench "$1" --way="-O2" --jmh="-wi $3 -i $2 -gc true -prof gc" --run
