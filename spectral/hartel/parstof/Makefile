TOP = ../../..
include $(TOP)/mk/boilerplate.mk

SRC_HC_OPTS += -cpp -XMagicHash -H80M

# not suitable for the fast/norm/slow treatment: GHC optimises
# the repeatition away.

include $(TOP)/mk/target.mk
