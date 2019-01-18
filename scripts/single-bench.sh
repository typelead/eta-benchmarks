#!/usr/bin/env sh

export TEST_NAME=${1}
export MEASUREMENT_ITERATIONS=${2}
export WORKER_ITERATIONS=${3}

etlas run eta-bench -- ${TEST_NAME} --way="-O2" --jmh="-wi ${WORKER_ITERATIONS} -i ${MEASUREMENT_ITERATIONS} -bm ss -gc true -prof gc" --run --compiler="./scripts/eta.sh"
