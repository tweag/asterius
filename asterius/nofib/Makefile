
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

