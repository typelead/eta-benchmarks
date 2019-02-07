# Notes

This file contains notes about benchmarks.

## Disabled

The following were disabled because of exceeding the 4G memory limit on CircleCI:

- `real/cacheprof`
- `shootout/fasta`
- `spectral/fish`

The following tests run too long (more than 2 hours) and have been moved into their own category (verySlow):

- `verySlow shootout/k-nucleotide`

Note: Some of the test actually fail with an *unexpected output* failure. For now we run all tests with --skip-check (there is an open issue for this).

## Port Status from NoFib

- `spectral/ansi` - Appears to require standard input (uses `interact`), but does not provide any input file.
- `spectral/fft2` - Appears to have a wide variety of outputs - hard to test. May be that we have incorrect floating point results.

- `spectral/expert` - Weird exception about a directory

- `spectral/simple` - Output *mostly* matches. It seems like we need to use `strictfp` to make things consistent.

- `shootout/k-nucleotide` - StackOverflowError in MemoryManager
- `shootout/reverse-complement` - Missing eta/bytestring/Utils
