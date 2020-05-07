TOP = ../..
include $(TOP)/mk/boilerplate.mk
-include opts.mk
include $(TOP)/mk/target.mk

CLEAN_FILES += $(wildcard banner.*stdin) $(wildcard banner.*stdout)

banner.faststdin : banner.stdin.template
	perl -0777pe '$$_=$$_ x 3500' banner.stdin.template > banner.faststdin

banner.stdin : banner.stdin.template
	perl -0777pe '$$_=$$_ x 35000' banner.stdin.template > banner.stdin

banner.slowstdin : banner.stdin.template
	perl -0777pe '$$_=$$_ x 180000' banner.stdin.template > banner.slowstdin

banner.faststdout : banner.stdout.template
	perl -0777pe '$$_=$$_ x 3500' banner.stdout.template > banner.faststdout

banner.stdout : banner.stdout.template
	perl -0777pe '$$_=$$_ x 35000' banner.stdout.template > banner.stdout

banner.slowstdout : banner.stdout.template
	perl -0777pe '$$_=$$_ x 180000' banner.stdout.template > banner.slowstdout

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

boot :: banner.$(INFIX)stdout banner.$(INFIX)stdin
