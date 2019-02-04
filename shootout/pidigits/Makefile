TOP = ../..
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/target.mk

FAST_OPTS = 2000
NORM_OPTS = 6000
SLOW_OPTS = 10000  # official shootout setting

SRC_RUNTEST_OPTS += +RTS -K512M -H1024M -M1024M -RTS

# The benchmark game also uses -fllvm, which we can't since it might
# not be available on the developer's machine.
HC_OPTS += -O2
