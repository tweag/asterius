TOP = ../..
include $(TOP)/mk/boilerplate.mk
-include opts.mk
include $(TOP)/mk/target.mk

# When you change these, make sure to update the repetition factor in the perl
# expression below
FAST_OPTS =   2000
NORM_OPTS =  20000
SLOW_OPTS = 100000

CLEAN_FILES += awards.stdout awards.slowstdout

# Keep the repetition factor (10 and 50) up to date with the OPTS above!
awards.stdout : awards.faststdout
	perl -0777pe '$$_=$$_ x 10' awards.faststdout > awards.stdout

awards.slowstdout : awards.faststdout
	perl -0777pe '$$_=$$_ x 50' awards.faststdout > awards.slowstdout

# Since we only decide here what the INFIX is, it's required to first run `make
# boot` and only then `make` with the same $mode (otherwise `make` doesn't
# "see" the file and doesn't call `runstdtest` correctly)
ifeq "$(mode)" "slow"
 INFIX = slow
else
 ifeq "$(mode)" "fast"
  INFIX = fast
 else
  INFIX =
 endif
endif

boot :: awards.$(INFIX)stdout
