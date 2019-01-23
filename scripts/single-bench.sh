#!/usr/bin/env sh

TEST_NAME=${1}
MEASUREMENT_ITERATIONS=${2}
WORKER_ITERATIONS=${3}

etlas run eta-bench -- "${TEST_NAME}" --way="-O2" --jmh="-wi ${WORKER_ITERATIONS} -i ${MEASUREMENT_ITERATIONS} -gc true -prof gc -f 1 -bm all" --run --compiler="./scripts/eta.sh"
