TOP = ../../..
include $(TOP)/mk/boilerplate.mk

SRC_HC_OPTS += -cpp -XMagicHash

FAST_OPTS = 400000
NORM_OPTS = 400000
SLOW_OPTS = 1500000 +RTS -K20M -RTS
SRC_RUNTEST_OPTS += +RTS -K25m -RTS

include $(TOP)/mk/target.mk
