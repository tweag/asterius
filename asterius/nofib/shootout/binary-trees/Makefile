TOP = ../..
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/target.mk

# First parameter controls number of iterations, second parameter controls depth of generated trees
# Official shootout setting: 1 20
# We do more iterations here for stability over different GC parameterisations
FAST_OPTS = 15 11
NORM_OPTS = 50 13
SLOW_OPTS = 60 14

# The benchmark game also uses -fllvm, which we can't since it might
# not be available on the developer's machine.
HC_OPTS += -funbox-strict-fields
SRC_RUNTEST_OPTS += +RTS -K128M -H200m -RTS
