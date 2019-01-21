# Notes

This file contains notes about benchmarks.

Please check `./scripts/benchmarks` to see the current configuration.

## Disabled

The following were disabled because of exceeding the 4G memory limit on CircleCI:

- `spectral/exact-reals` (Even on slow benchmarks)

- `imaginary/digits-of-e1`
- `imaginary/digits-of-e2`
- `spectral/clausify`
- `spectral/constraints`
- `shootout/pidigits`
- `shootout/n-body`

The following were disabled because of taking too much time:

- `spectral/circsim`
- `smp/threads001`

The following were disabled because of failure:

- `imaginary/integrate` - The output is coming out as `--0.0` instead of `0.0`.
- `spectral/hartel/event` - The runtime fails with a space leak


## Port Status from NoFib

- `spectral/ansi` - Appears to require standard input (uses `interact`), but does not provide any input file.
- `spectral/fft2` - Appears to have a wide variety of outputs - hard to test. May be that we have incorrect floating point results.

- `spectral/expert` - Weird exception about a directory

- `spectral/simple` - Output *mostly* matches. It seems like we need to use `strictfp` to make things consistent.

- `shootout/k-nucleotide` - StackOverflowError in MemoryManager
- `shootout/reverse-complement` - Missing eta/bytestring/Utils
