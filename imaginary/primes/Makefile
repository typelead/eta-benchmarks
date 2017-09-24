TOP = ../..
include $(TOP)/mk/boilerplate.mk

# Override default SRCS, or we get cseive.c as well.
SRCS = Main.hs
SRC_RUNTEST_OPTS = +RTS -K1200k -RTS

include $(TOP)/mk/target.mk

FAST_OPTS = 1500
NORM_OPTS = 1500
SLOW_OPTS = 5500
