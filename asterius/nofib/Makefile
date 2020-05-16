
clean:
	$(RM) -fr ./*/*/*.hi
	$(RM) -fr ./*/*/*.o
	# $(RM) -fr ./*/*/*.ghc.stdout
	# $(RM) -fr ./*/*/*.ahc.stdout
	$(RM) -fr ./*/*/Main
	$(RM) real/fulsom/Bah           # Remove these separately
	$(RM) real/compress/Lzw         # Remove these separately
	$(RM) real/maillist/runtime_files/fast.tex # God knows what this is
	$(RM) real/maillist/runtime_files/norm.tex # God knows what this is
	$(RM) real/maillist/runtime_files/slow.tex # God knows what this is
	# node files
	$(RM) -r */*/node
	$(RM) */*/Main.mjs
	$(RM) */*/Main.req.mjs
	$(RM) */*/Main.wasm
	$(RM) */*/Main.wasm.mjs
	$(RM) */*/rts.autoapply.mjs
	$(RM) */*/rts.bytestring.mjs
	$(RM) */*/rts.closuretypes.mjs
	$(RM) */*/rts.constants.mjs
	$(RM) */*/rts.eventlog.mjs
	$(RM) */*/rts.exception.mjs
	$(RM) */*/rts.exports.mjs
	$(RM) */*/rts.float.mjs
	$(RM) */*/rts.fs.mjs
	$(RM) */*/rts.funtypes.mjs
	$(RM) */*/rts.gc.mjs
	$(RM) */*/rts.heapalloc.mjs
	$(RM) */*/rts.integer.mjs
	$(RM) */*/rts.memory.mjs
	$(RM) */*/rts.memorytrap.mjs
	$(RM) */*/rts.messages.mjs
	$(RM) */*/rts.mjs
	$(RM) */*/rts.modulify.mjs
	$(RM) */*/rts.node.mjs
	$(RM) */*/rts.reentrancy.mjs
	$(RM) */*/rts.scheduler.mjs
	$(RM) */*/rts.stablename.mjs
	$(RM) */*/rts.stableptr.mjs
	$(RM) */*/rts.staticptr.mjs
	$(RM) */*/rts.text.mjs
	$(RM) */*/rts.time.mjs
	$(RM) */*/rts.tracing.mjs
	$(RM) */*/rts.unicode.mjs

clean-ghc: clean
	$(RM) -fr ./*/*/*.ghc.stdout

clean-ahc: clean
	$(RM) -fr ./*/*/*.ahc.stdout

distclean: clean-ghc clean-ahc

