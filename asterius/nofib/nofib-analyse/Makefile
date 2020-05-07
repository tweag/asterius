TOP=..
include $(TOP)/mk/boilerplate.mk

PROG = nofib-analyse

$(PROG): *.hs
	cabal v2-build -w $(BOOT_HC)
	cp $$(cabal v2-exec -w $(BOOT_HC) -- which $(PROG)) $(PROG)

all :: $(PROG)

boot :: $(PROG)

# This clean hierarchy mirrors mk/ghc-target.mk.
# See mk/ghc-paths.mk for the semantics.

.PHONY: mostlyclean clean distclean maintainer-clean

mostlyclean::
	rm -rf dist-newstyle dist .ghc.environment.*

clean:: mostlyclean
	rm -f $(PROG)

distclean:: clean
maintainer-clean:: distclean
