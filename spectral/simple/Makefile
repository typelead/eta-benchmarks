TOP = ../..
include $(TOP)/mk/boilerplate.mk

# Without this we get the C version too!
SRCS = Main.hs

# Main is rather big
Main_HC_OPTS = -H110m

SRC_RUNTEST_OPTS += +RTS -K512M -H1024M -M1024M -RTS

include $(TOP)/mk/target.mk
