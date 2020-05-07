TOP = ../..
include $(TOP)/mk/boilerplate.mk
-include opts.mk
include $(TOP)/mk/target.mk

# I highly doubt that the current stdin file (which is just `grip-in` from the
# README) touches enough of the prover to be meaningful as a benchmark. But
# without any reference on how to construct a proper input, I see no way to
# make use this benchmark other than just iterating it more often.
FAST_OPTS =   400
NORM_OPTS =  4000
SLOW_OPTS = 20000
