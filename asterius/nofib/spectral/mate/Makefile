TOP = ../..
include $(TOP)/mk/boilerplate.mk

# It's necessary to specify those manually so that the current make-based system
# compiles them in the right order.
# TODO(michalt): This should go away once we move to the Shake-based system.
HS_SRCS = Board.hs Move.hs Problem.hs Solution.hs Main.hs

# 	Other problems
# ellerman.prob heathcote3.prob kidson.prob marin.prob \
# 		simple.prob fridlizius.prob holzhausen.prob \
#		kohtz.prob shinkman.prob wurzburg.prob

include $(TOP)/mk/target.mk

FAST_OPTS = runtime_files/heathcote3.prob
NORM_OPTS = runtime_files/kohtz.prob
SLOW_OPTS = runtime_files/holzhausen.prob
