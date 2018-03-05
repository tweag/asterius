# Asterius: A Haskell to WebAssembly compiler

[![CircleCI](https://circleci.com/gh/tweag/asterius/tree/master.svg?style=shield)](https://circleci.com/gh/tweag/asterius/tree/master)

A Haskell to WebAssembly compiler. Project status: **pre-alpha**.

What's already present:

* A framework that hijacks the normal `ghc` pipeline and retrieves an
  in-memory representation of raw Cmm.
* A test suite for booting that compiles `ghc-prim`, `integer-simple` and `base`.
* Complete raw Haskell bindings of `binaryen`, in the `hbinaryen` package.
* A serializable IR, roughly mapping to `binaryen` IR.

Currently working on:

* A custom linker that merges modules, handles relocations and supports tail-calls.

What comes next:

* An RTS written from scratch in native WebAssembly. Will implement
  enough primops & libc stubs for an MVP(Minimum Viable Product).
* A test suite for the generated WebAssembly code.

## Building

Assumes x64 platform, only tested on Linux for now. Dependencies:

* `cmake`/`make`/`g++`: For building in-tree [`binaryen`](https://github.com/WebAssembly/binaryen)
* `autoconf`/`sed`: For booting `ghc-prim`/`base`
* `nodejs`: For running tests
* `ghc-head` built with `TABLES_NEXT_TO_CODE` disabled and integer library set to `integer-simple`. Get one via `docker pull terrorjack/meikyu:ghc-head`
* `stack`. `cabal` users need to run `hpack` first.

Simply run `stack build asterius`. Set `MAKEFLAGS=-j8` to pass flags to `make` for parallel building of `binaryen`. Run `stack test asterius:ahc-boot` to test if booting works.

Windows users need to run it in an `mingw-w64` Win64 shell. `stack` takes care of installing/launching it by default.

## Differences with [WebGHC](https://webghc.github.io/)

* Doesn't depend on Emscripten/LLVM. There is no plan to port the C runtime and support C libraries, at least for now.

## Sponsors

&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;
[![Tweag I/O](http://i.imgur.com/0HK8X4y.png)](http://tweag.io)

Asterius is maintained by [Tweag I/O](http://tweag.io/).

Have questions? Need help? Tweet at
[@tweagio](http://twitter.com/tweagio).
