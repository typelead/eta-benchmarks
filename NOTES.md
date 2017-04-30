# Notes

This file contains notes about benchmarks.

# Enabled
- `shootout/n-body` tickles the edge case in Eta's tail call handling - requires almost 285m of stack to work.

# Disabled

The following were disabled because of exceeding the 4G memory limit on CircleCI:

- `imaginary/digits-of-e1`
- `imaginary/digits-of-e2`
- `spectral/clausify`
- `spectral/constraints`
- `shootout/pidigits`

The following were disabled because of taking too much time:

- `spectral/circsim`

The following were disabled because of failure:

- `imaginary/integrate` - The output is coming out as `--0.0` instead of `0.0`.
