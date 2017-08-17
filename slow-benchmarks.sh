#!/usr/bin/env sh

etlas install transformers array containers bytestring
stack install
eta-bench clean
eta-bench imaginary/digits-of-e1 imaginary/digits-of-e2 shootout/pidigits spectral/circsim spectral/clausify spectral/constraints shootout/n-body smp/threads001 --way="-O2" --jmh="-wi 10 -i 10" --run
