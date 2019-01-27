# The Eta Benchmark Suite

[![Build Status](https://circleci.com/gh/typelead/eta-benchmarks.svg?style=shield&circle-token=97b838c8a686e8a33747d105d1713f970c381680)](https://circleci.com/gh/typelead/eta-benchmarks)

This repository contains a set of performance tests used to properly measure how changes in the compiler affect performance. A big part of this suite consists of  the `nofib` benchmark suite along with benchmarks that are relevant for Eta.

## Methodology
The [Java Micobenchmarking Harness](http://openjdk.java.net/projects/code-tools/jmh/) is used to conduct the benchmarks. The follow process occurs when you run a benchmark:

1. The benchmark is compiled into a JAR file.
2. The `eta.main.main` method (the entry point into Eta programs) is invoked from the
   JMH framework.
3. The JMH framework will fork a single JVM process and run the program 10 times for
   warmup and 10 times for measurement and the average/error is calculated.
   - At the end of each iteration, GC is run and all static thunks are reset to unevaluated state.

## Getting Started

### Prerequisites
- Gradle
- JDK 1.7+
- etlas

### Runner Installation

First, get setup:

```
./scripts/init.sh
```

### Quick Run

A standalone script will run the fast part of the suite, also run by CircleCI.

```
./scripts/fast-benchmarks.sh
```

## Slow Run

A standalone script will run the slow part of the suite.

```
./scripts/slow-benchmarks.sh
```

## Run a Single Benchmark

A single benchmark can be run:

```
./scripts/single-bench.sh [test-name] [measurement-iterations] [worker-iterations]

```

Example:

```
./scripts/single-bench.sh imaginary/bernouilli 10 5

```
This indicates 10 measurement iterations and 5 warmup iterations.

If you want to do more fine-grained performance benchmarking, please see the remaining sections.

## Running

All runs of the runner should be at the root of the repository.

### To run an individual test

`$ eta-bench [test-name] --run`

Example:

`$ eta-bench scs --run`

### To run an entire suite

`$ eta-bench [suite-name] --run`

Example:

`$ eta-bench imaginary --run`

### Clean up test/benchmark artifacts

`$ eta-bench clean`

## Contact Us

If you had trouble using this project, you can give us feedback by:

- filing an [issue](https://github.com/typelead/eta-benchmarks/issues/new)

- discussing with us on [Gitter](https://gitter.im/typelead/eta)
