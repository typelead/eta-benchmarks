#!/usr/bin/env sh

etlas install transformers array containers bytestring random
stack install
eta-bench clean
