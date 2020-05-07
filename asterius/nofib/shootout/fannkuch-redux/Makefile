TOP = ../..
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/target.mk

# Time grows exponentially in the argument with base 12
FAST_OPTS = 10 # 0.24s in 2018
NORM_OPTS = 11 # 3.00s in 2018
SLOW_OPTS = 11
# Official shootout setting
# SLOW_OPTS = 12 # 40s in 2018

# The benchmark game uses -fllvm, which we can't since it might
# not be available on the developer's machine.
