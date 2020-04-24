## High-level architecture

The `asterius` project is hosted at
[GitHub](https://github.com/tweag/asterius). The monorepo contains several
packages:

* [`asterius`](https://github.com/tweag/asterius/tree/master/asterius).
  This is the central package of the `asterius` compiler.

* [`binaryen`](https://github.com/tweag/asterius/tree/master/binaryen).
  It contains the latest source code of the C++ library
  [`binaryen`](https://github.com/WebAssembly/binaryen) in tree, and provides
  complete raw bindings to its
  [C API](https://github.com/tweag/asterius/blob/master/binaryen/binaryen/src/binaryen-c.h).

* [`ghc-toolkit`](https://github.com/tweag/asterius/tree/master/ghc-toolkit).
  It provides a framework for implementing Haskell-to-X compilers by retrieving
  `ghc`'s various types of in-memory intermediate representations. It also
  contains the latest source code of `ghc-prim`/`integer-gmp`/`integer-simple`/`base`
  in tree.

* [`wasm-toolkit`](https://github.com/tweag/asterius/tree/master/wasm-toolkit).
  It implements the WebAssembly AST and binary encoder/decoder in Haskell, and
  is now the default backend for generating WebAssembly binary code.

The `asterius` package provides an `ahc` executable which is a drop-in
replacement of `ghc` to be used with `Setup configure`. `ahc` redirects all
arguments to the real `ghc` most of the time, but when it's invoked with the
`--make` major mode, it invokes `ghc` with its frontend plugin. This is
inspired by Edward Yang's
[How to integrate GHC API programs with Cabal](http://blog.ezyang.com/2017/02/how-to-integrate-ghc-api-programs-with-cabal/).

Based on `ghc-toolkit`, `asterius` implements a
[`ghc` frontend plugin](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/extending_ghc.html#frontend-plugins)
which translates
[Cmm](https://ghc.haskell.org/trac/ghc/wiki/Commentary/Compiler/CmmType) to
`binaryen` IR. The serialized `binaryen` IR can then be loaded and linked to a
WebAssembly binary (not implemented yet). The normal compilation pipeline which
generates native machine code is not affected.

## About "booting"

In order for `asterius` to support non-trivial Haskell programs (that is, at
least most things in `Prelude`), it needs to run the compilation process for
`base` and its dependent packages. This process is known as "booting".

The `asterius` package provides an `ahc-boot` test suite which tests booting by
compiling the wired-in packages provided by `ghc-toolkit` and using `ahc` to
replace `ghc` when configuring. This is inspired by Joachim Breitner's
[`veggies`](https://github.com/nomeata/veggies).
