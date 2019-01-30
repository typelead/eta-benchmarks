#!/usr/bin/env sh

MEASUREMENT_ITERATIONS=5
WARMUP_ITERATIONS=1

./scripts/single-bench.sh conc/chan ${MEASUREMENT_ITERATIONS} ${WARMUP_ITERATIONS}
./scripts/single-bench.sh imaginary/bernouilli ${MEASUREMENT_ITERATIONS} ${WARMUP_ITERATIONS}
./scripts/single-bench.sh imaginary/digits-of-e1 ${MEASUREMENT_ITERATIONS} ${WARMUP_ITERATIONS}
./scripts/single-bench.sh imaginary/digits-of-e2 ${MEASUREMENT_ITERATIONS} ${WARMUP_ITERATIONS}
./scripts/single-bench.sh imaginary/exp3_8 ${MEASUREMENT_ITERATIONS} ${WARMUP_ITERATIONS}
./scripts/single-bench.sh imaginary/gen_regexps ${MEASUREMENT_ITERATIONS} ${WARMUP_ITERATIONS}
./scripts/single-bench.sh imaginary/integrate ${MEASUREMENT_ITERATIONS} ${WARMUP_ITERATIONS}
./scripts/single-bench.sh imaginary/kahan ${MEASUREMENT_ITERATIONS} ${WARMUP_ITERATIONS}
./scripts/single-bench.sh imaginary/paraffins ${MEASUREMENT_ITERATIONS} ${WARMUP_ITERATIONS}
./scripts/single-bench.sh imaginary/primes ${MEASUREMENT_ITERATIONS} ${WARMUP_ITERATIONS}
./scripts/single-bench.sh imaginary/queens ${MEASUREMENT_ITERATIONS} ${WARMUP_ITERATIONS}
./scripts/single-bench.sh imaginary/rfib ${MEASUREMENT_ITERATIONS} ${WARMUP_ITERATIONS}
./scripts/single-bench.sh imaginary/tak ${MEASUREMENT_ITERATIONS} ${WARMUP_ITERATIONS}
./scripts/single-bench.sh imaginary/wheel-sieve1 ${MEASUREMENT_ITERATIONS} ${WARMUP_ITERATIONS}
./scripts/single-bench.sh imaginary/wheel-sieve2 ${MEASUREMENT_ITERATIONS} ${WARMUP_ITERATIONS}
./scripts/single-bench.sh imaginary/x2n1 ${MEASUREMENT_ITERATIONS} ${WARMUP_ITERATIONS}
./scripts/single-bench.sh real/anna ${MEASUREMENT_ITERATIONS} ${WARMUP_ITERATIONS}
./scripts/single-bench.sh real/bspt ${MEASUREMENT_ITERATIONS} ${WARMUP_ITERATIONS}
./scripts/single-bench.sh real/cacheprof ${MEASUREMENT_ITERATIONS} ${WARMUP_ITERATIONS}
./scripts/single-bench.sh real/compress ${MEASUREMENT_ITERATIONS} ${WARMUP_ITERATIONS}
./scripts/single-bench.sh shootout/binary-trees ${MEASUREMENT_ITERATIONS} ${WARMUP_ITERATIONS}
./scripts/single-bench.sh shootout/fannkuch-redux ${MEASUREMENT_ITERATIONS} ${WARMUP_ITERATIONS}
# Unknown - Slow - ./scripts/single-bench.sh shootout/fasta ${MEASUREMENT_ITERATIONS} ${WARMUP_ITERATIONS}
# Unknown - slow - ./scripts/single-bench.sh shootout/k-nucleotide ${MEASUREMENT_ITERATIONS} ${WARMUP_ITERATIONS}
./scripts/single-bench.sh shootout/n-body ${MEASUREMENT_ITERATIONS} ${WARMUP_ITERATIONS}
./scripts/single-bench.sh shootout/pidigits ${MEASUREMENT_ITERATIONS} ${WARMUP_ITERATIONS}
./scripts/single-bench.sh shootout/spectral-norm ${MEASUREMENT_ITERATIONS} ${WARMUP_ITERATIONS}
./scripts/single-bench.sh spectral/atom ${MEASUREMENT_ITERATIONS} ${WARMUP_ITERATIONS}
./scripts/single-bench.sh spectral/awards ${MEASUREMENT_ITERATIONS} ${WARMUP_ITERATIONS}
./scripts/single-bench.sh spectral/banner ${MEASUREMENT_ITERATIONS} ${WARMUP_ITERATIONS}
./scripts/single-bench.sh spectral/boyer ${MEASUREMENT_ITERATIONS} ${WARMUP_ITERATIONS}
./scripts/single-bench.sh spectral/boyer2 ${MEASUREMENT_ITERATIONS} ${WARMUP_ITERATIONS}
./scripts/single-bench.sh spectral/calendar ${MEASUREMENT_ITERATIONS} ${WARMUP_ITERATIONS}
./scripts/single-bench.sh spectral/cichelli ${MEASUREMENT_ITERATIONS} ${WARMUP_ITERATIONS}
./scripts/single-bench.sh spectral/circsim ${MEASUREMENT_ITERATIONS} ${WARMUP_ITERATIONS}
./scripts/single-bench.sh spectral/clausify ${MEASUREMENT_ITERATIONS} ${WARMUP_ITERATIONS}
./scripts/single-bench.sh spectral/constraints ${MEASUREMENT_ITERATIONS} ${WARMUP_ITERATIONS}
./scripts/single-bench.sh spectral/cryptarithm1 ${MEASUREMENT_ITERATIONS} ${WARMUP_ITERATIONS}
./scripts/single-bench.sh spectral/cryptarithm2 ${MEASUREMENT_ITERATIONS} ${WARMUP_ITERATIONS}
./scripts/single-bench.sh spectral/cse ${MEASUREMENT_ITERATIONS} ${WARMUP_ITERATIONS}
./scripts/single-bench.sh spectral/eliza ${MEASUREMENT_ITERATIONS} ${WARMUP_ITERATIONS}
./scripts/single-bench.sh spectral/exact-reals ${MEASUREMENT_ITERATIONS} ${WARMUP_ITERATIONS}
# NPE - ./scripts/single-bench.sh spectral/expert ${MEASUREMENT_ITERATIONS} ${WARMUP_ITERATIONS}
./scripts/single-bench.sh spectral/fibheaps ${MEASUREMENT_ITERATIONS} ${WARMUP_ITERATIONS}
./scripts/single-bench.sh spectral/fish ${MEASUREMENT_ITERATIONS} ${WARMUP_ITERATIONS}
./scripts/single-bench.sh spectral/gcd ${MEASUREMENT_ITERATIONS} ${WARMUP_ITERATIONS}
./scripts/single-bench.sh spectral/hartel/comp_lab_zift ${MEASUREMENT_ITERATIONS} ${WARMUP_ITERATIONS}
./scripts/single-bench.sh spectral/hartel/event ${MEASUREMENT_ITERATIONS} ${WARMUP_ITERATIONS}
./scripts/single-bench.sh spectral/hartel/fft ${MEASUREMENT_ITERATIONS} ${WARMUP_ITERATIONS}
./scripts/single-bench.sh spectral/hartel/genfft ${MEASUREMENT_ITERATIONS} ${WARMUP_ITERATIONS}
# Unknown - Slow - ./scripts/single-bench.sh spectral/hartel/ida ${MEASUREMENT_ITERATIONS} ${WARMUP_ITERATIONS}
./scripts/single-bench.sh spectral/hartel/listcompr ${MEASUREMENT_ITERATIONS} ${WARMUP_ITERATIONS}
./scripts/single-bench.sh spectral/hartel/listcopy ${MEASUREMENT_ITERATIONS} ${WARMUP_ITERATIONS}
./scripts/single-bench.sh spectral/hartel/nucleic2 ${MEASUREMENT_ITERATIONS} ${WARMUP_ITERATIONS}
./scripts/single-bench.sh spectral/hartel/parstof ${MEASUREMENT_ITERATIONS} ${WARMUP_ITERATIONS}
./scripts/single-bench.sh spectral/hartel/sched ${MEASUREMENT_ITERATIONS} ${WARMUP_ITERATIONS}
./scripts/single-bench.sh spectral/hartel/solid ${MEASUREMENT_ITERATIONS} ${WARMUP_ITERATIONS}
./scripts/single-bench.sh spectral/hartel/transform ${MEASUREMENT_ITERATIONS} ${WARMUP_ITERATIONS}
./scripts/single-bench.sh spectral/hartel/typecheck ${MEASUREMENT_ITERATIONS} ${WARMUP_ITERATIONS}
./scripts/single-bench.sh spectral/hartel/wang ${MEASUREMENT_ITERATIONS} ${WARMUP_ITERATIONS}
./scripts/single-bench.sh spectral/hartel/wave4main ${MEASUREMENT_ITERATIONS} ${WARMUP_ITERATIONS}
./scripts/single-bench.sh spectral/integer ${MEASUREMENT_ITERATIONS} ${WARMUP_ITERATIONS}
./scripts/single-bench.sh spectral/knights ${MEASUREMENT_ITERATIONS} ${WARMUP_ITERATIONS}
./scripts/single-bench.sh spectral/lambda ${MEASUREMENT_ITERATIONS} ${WARMUP_ITERATIONS}
./scripts/single-bench.sh spectral/last-piece ${MEASUREMENT_ITERATIONS} ${WARMUP_ITERATIONS}
./scripts/single-bench.sh spectral/lcss ${MEASUREMENT_ITERATIONS} ${WARMUP_ITERATIONS}
./scripts/single-bench.sh spectral/life ${MEASUREMENT_ITERATIONS} ${WARMUP_ITERATIONS}
./scripts/single-bench.sh spectral/mandel ${MEASUREMENT_ITERATIONS} ${WARMUP_ITERATIONS}
./scripts/single-bench.sh spectral/mandel2 ${MEASUREMENT_ITERATIONS} ${WARMUP_ITERATIONS}
./scripts/single-bench.sh spectral/mate ${MEASUREMENT_ITERATIONS} ${WARMUP_ITERATIONS}
./scripts/single-bench.sh spectral/minimax ${MEASUREMENT_ITERATIONS} ${WARMUP_ITERATIONS}
./scripts/single-bench.sh spectral/multiplier ${MEASUREMENT_ITERATIONS} ${WARMUP_ITERATIONS}
./scripts/single-bench.sh spectral/para ${MEASUREMENT_ITERATIONS} ${WARMUP_ITERATIONS}
./scripts/single-bench.sh spectral/power ${MEASUREMENT_ITERATIONS} ${WARMUP_ITERATIONS}
./scripts/single-bench.sh spectral/primetest ${MEASUREMENT_ITERATIONS} ${WARMUP_ITERATIONS}
./scripts/single-bench.sh spectral/puzzle ${MEASUREMENT_ITERATIONS} ${WARMUP_ITERATIONS}
./scripts/single-bench.sh spectral/rewrite ${MEASUREMENT_ITERATIONS} ${WARMUP_ITERATIONS}
./scripts/single-bench.sh spectral/scc ${MEASUREMENT_ITERATIONS} ${WARMUP_ITERATIONS}
./scripts/single-bench.sh spectral/secretary ${MEASUREMENT_ITERATIONS} ${WARMUP_ITERATIONS}
./scripts/single-bench.sh spectral/simple ${MEASUREMENT_ITERATIONS} ${WARMUP_ITERATIONS}
./scripts/single-bench.sh spectral/sorting ${MEASUREMENT_ITERATIONS} ${WARMUP_ITERATIONS}
./scripts/single-bench.sh spectral/sphere ${MEASUREMENT_ITERATIONS} ${WARMUP_ITERATIONS}
./scripts/single-bench.sh spectral/treejoin ${MEASUREMENT_ITERATIONS} ${WARMUP_ITERATIONS}
