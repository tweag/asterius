TOP = ../..
include $(TOP)/mk/boilerplate.mk

# Without this we get the C version too!
SRCS = Main.hs

# Main is rather big
Main_HC_OPTS = -H110m

include $(TOP)/mk/target.mk

FAST_OPTS = 1
NORM_OPTS = 20
SLOW_OPTS = 100
