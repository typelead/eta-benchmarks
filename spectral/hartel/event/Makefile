TOP = ../../..
include $(TOP)/mk/boilerplate.mk

SRC_HC_OPTS += -cpp -XMagicHash

FAST_OPTS = 400000
NORM_OPTS = 400000
SLOW_OPTS = 1500000 +RTS -K512M -RTS

SRC_RUNTEST_OPTS += +RTS -K512M -RTS

include $(TOP)/mk/target.mk
