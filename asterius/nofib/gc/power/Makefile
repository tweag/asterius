TOP = ../..
include $(TOP)/mk/boilerplate.mk
-include opts.mk
include $(TOP)/mk/target.mk

FAST_OPTS = 50
NORM_OPTS = 80
SLOW_OPTS = 90

ifeq "$(HEAP)" "LARGE"
SRC_RUNTEST_OPTS += +RTS -H16m -RTS
endif
ifeq "$(HEAP)" "OLD"
SRC_RUNTEST_OPTS += +RTS -H10m -RTS
endif
