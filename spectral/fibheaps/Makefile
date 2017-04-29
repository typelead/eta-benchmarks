TOP = ../..
include $(TOP)/mk/boilerplate.mk

FAST_OPTS = 5000
NORM_OPTS = 5000
SLOW_OPTS = 60000
HC_OPTS += -package array
SRC_RUNTEST_OPTS += +RTS -K3m -RTS

include $(TOP)/mk/target.mk
