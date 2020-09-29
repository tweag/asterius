
clean: clean-rts
	$(RM) -fr ./*/*/*.hi
	$(RM) -fr ./*/*/*.o
	$(RM) -fr ./*/*/Main
	$(RM) ./real/fulsom/Bah                      # Remove these separately
	$(RM) ./real/compress/Lzw                    # Remove these separately
	$(RM) ./real/maillist/runtime_files/fast.tex # Remove these separately
	$(RM) ./real/maillist/runtime_files/norm.tex # Remove these separately
	$(RM) ./real/maillist/runtime_files/slow.tex # Remove these separately
	$(RM) -fr ./*/*/node
	$(RM) ./*/*/Main.mjs
	$(RM) ./*/*/Main.req.mjs
	$(RM) ./*/*/Main.wasm
	$(RM) ./*/*/Main.wasm.mjs
	$(RM) ./*/*/default.mjs

clean-rts:
	$(RM) ./*/*/rts.mjs
	$(RM) ./*/*/rts.*.mjs

clean-ghc: clean
	$(RM) -fr ./*/*/*.ghc.stdout
	$(RM) -fr ./*/*/*.ghc.stderr

clean-ahc: clean
	$(RM) -fr ./*/*/*.ahc.stdout
	$(RM) -fr ./*/*/*.ahc.stderr

distclean: clean-ghc clean-ahc

remove-local-failures:
	$(RM) -fr ./shootout/pidigits
	$(RM) -fr ./real/eff-VS
	$(RM) -fr ./real/pic
	$(RM) -fr ./real/scs
	$(RM) -fr ./shootout/fannkuch-redux
	$(RM) -fr ./shootout/k-nucleotide

revive-local-failures:
	git checkout -- ./shootout/pidigits
	git checkout -- ./real/eff-VS
	git checkout -- ./real/pic
	git checkout -- ./real/scs
	git checkout -- ./shootout/fannkuch-redux
	git checkout -- ./shootout/k-nucleotide

