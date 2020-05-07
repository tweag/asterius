TOP=../..
include $(TOP)/mk/boilerplate.mk

FAST_OPTS = 1 2 2000 1000 1001 2000
NORM_OPTS = 1 2 2000 1000 1001 4000
SLOW_OPTS = 1 2 4000 1000 1001 4000

ifeq "$(HEAP)" "LARGE"
SRC_RUNTEST_OPTS += +RTS -H256m -RTS
endif

include $(TOP)/mk/target.mk
