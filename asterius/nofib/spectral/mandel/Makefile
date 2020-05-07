TOP = ../..
include $(TOP)/mk/boilerplate.mk

SRC_RUNTEST_OPTS += -stdout-binary

# Override SRCS so that we don't link in MandelOld.hs
SRCS = Main.hs Mandel.lhs PortablePixmap.lhs

include $(TOP)/mk/target.mk

FAST_OPTS = -2.0 -2.0 2.0 2.0  25  25 75
NORM_OPTS = -2.0 -2.0 2.0 2.0  80  80 75
SLOW_OPTS = -2.0 -2.0 2.0 2.0 180 180 75
