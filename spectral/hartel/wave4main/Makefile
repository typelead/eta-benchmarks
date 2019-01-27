TOP = ../../..
include $(TOP)/mk/boilerplate.mk

SRC_HC_OPTS += -cpp -XMagicHash

FAST_OPTS = 4000
NORM_OPTS = 4000
SLOW_OPTS = 6000

SRC_RUNTEST_OPTS = +RTS -K512m -RTS

include $(TOP)/mk/target.mk
