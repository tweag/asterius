TOP = ../..
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/target.mk

# The core (Dom.hs) is taken from the dom-lt package.

# Compute dominators over a graph
# Graphs are real examples for control flow graphs produced by GHC.
# First parameter controls input file, second parameter controls repetitions.
# We do more iterations here for stability over different GC parameterisations
# and to shift compute time away from read/graph creation.
FAST_OPTS = ghc-examples.in 50
NORM_OPTS = ghc-examples.in 400
SLOW_OPTS = ghc-examples.in 1800

# We require containers.
HC_OPTS += -package containers
