#!/usr/bin/env sh

TEST_NAME=${1}
MEASUREMENT_ITERATIONS=${2}
WARMUP_ITERATIONS=${3}

etlas run eta-bench -- "${TEST_NAME}" --way="-O2" --jmh="-i ${MEASUREMENT_ITERATIONS} -wi ${WARMUP_ITERATIONS} -gc true -prof gc -f 1 -bm all" --run --compiler="./scripts/eta.sh" --skip-check
