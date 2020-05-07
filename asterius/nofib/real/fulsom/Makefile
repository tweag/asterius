TOP = ../..
include $(TOP)/mk/boilerplate.mk

SRC_HC_OPTS += -cpp

# Bah.hs is a test file, which we don't want in SRCS
EXCLUDED_SRCS = Bah.hs

FAST_OPTS = 4
NORM_OPTS = 5
SLOW_OPTS = 6

include $(TOP)/mk/target.mk

