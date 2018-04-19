## Reading list

Here is a brief list of relevant readings about GHC internals and WebAssembly suited for newcomers.

* [GHC documentation regarding the GHC API](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/extending_ghc.html): a nice reading for anyone looking forward to using the GHC API.
* [GHC commentary](https://ghc.haskell.org/trac/ghc/wiki/Commentary): a wiki containing lots of additional knowledge regarding GHC's implementation. Keep in mind some content is out-dated though. Some useful entries regarding this project:
    * [Building guide](https://ghc.haskell.org/trac/ghc/wiki/Building). A tl;dr for this section is our CI scripts.
    * [Overview of pipeline](https://ghc.haskell.org/trac/ghc/wiki/Commentary/Pipeline): we use the Hooks mechanism (specifically, `runPhaseHook`) to replace the default pipeline with our own, to enable manipulation of in-memory IRs.
    * [How STG works](https://ghc.haskell.org/trac/ghc/wiki/Commentary/Compiler/GeneratedCode): a nice tutorial containing several examples of compiled examples, illustrating how the generated code works under the hood.
    * [The Cmm types](https://ghc.haskell.org/trac/ghc/wiki/Commentary/Compiler/CmmType): it's outdated and the types don't exactly match the GHC codebase now, but the explanations still shed some light on how the current Cmm types work.
    * [The runtime system](https://ghc.haskell.org/trac/ghc/wiki/Commentary/Rts): content regarding the runtime system.
* [Understanding the Stack](http://www.well-typed.com/blog/94/): A blog post explaining how generated code works at the assembly level. Also, its sequel [Understanding the RealWorld](http://www.well-typed.com/blog/95/)
* [The WebAssembly spec](https://webassembly.github.io/spec/core/index.html): a useful reference regarding what's already present in WebAssembly.
* [The `binaryen` C API](https://github.com/WebAssembly/binaryen/blob/master/src/binaryen-c.h): `binaryen` handles WebAssembly code generation. There are a few differences regarding `binaryen` AST and WebAssembly AST, the most notable ones:
    * `binaryen` uses a recursive `BinaryenExpression` which is side-effectful. The original WebAssembly standard instead uses a stack-based model and manipulates the operand stack with instructions.
    * `binaryen` contains a "Relooper" which can recover high-level structured control flow from a CFG. However the relooper doesn't handle jumping to unknown labels (aka computed goto), so we don't use it to handle tail calls.

The following entries are papers which consume much more time to read, but still quite useful for newcomers:

* [Making a fast curry: push/enter vs. eval/apply for higher-order languages](https://www.microsoft.com/en-us/research/publication/make-fast-curry-pushenter-vs-evalapply/): A thorough explanation of what is STG and how it is implemented (via two different groups of rewrite rules, also with real benchmarks)
* [The STG runtime system (revised)](https://github.com/ghc/ghc/blob/master/docs/rts/rts.tex): Includes some details on the runtime system and worth a read. It's a myth why it's not merged with the commentary though. Install a TeX distribution like TeX Live or use a service like Overleaf to compile the `.tex` file to `.pdf` before reading.
* [The GHC storage manager](https://github.com/ghc/ghc/blob/master/docs/storage-mgt/sm.tex): Similar to above.
* [Bringing the Web up to Speed with WebAssembly](https://github.com/WebAssembly/spec/blob/master/papers/pldi2017.pdf): The PLDI'17 paper about WebAssembly. Contains overview of WebAssembly design rationales and rules of small-step operational semantics.

Finally, the GHC codebase itself is also a must-read, but since it's huge we only need to check relevant parts when unsure about its behavior. Tips on reading GHC code:

* There are a lot of insightful and up-to-date comments which all begin with "Notes on xxx". It's a pity the notes are neither collected into the sphinx-generated documentation or into the haddock docs of GHC API.
* When writing `build.mk` for compiling GHC, add `HADDOCK_DOCS = YES` to ensure building haddock docs of GHC API, and `EXTRA_HADDOCK_OPTS += --quickjump --hyperlinked-source` to enable symbol hyperlinks in the source pages. This will save you tons of time from `grep`ing the ghc codebase.
* `grep`ing is still unavoidable in some cases, since there's a lot of CPP involved and they aren't well handled by haddock.
