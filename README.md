# Asterius: A Haskell to WebAssembly compiler

[![CircleCI](https://circleci.com/gh/tweag/asterius/tree/master.svg?style=shield)](https://circleci.com/gh/tweag/asterius/tree/master)
[![AppVeyor](https://ci.appveyor.com/api/projects/status/github/tweag/asterius?branch=master&svg=true)](https://ci.appveyor.com/project/TerrorJack/asterius?branch=master)

A Haskell to WebAssembly compiler. Project status: **pre-alpha**.

What's already present:

* A framework that hijacks the normal `ghc` pipeline and retrieves an
  in-memory representation of raw Cmm.
* A test suite for booting that compiles `ghc-prim`, `integer-simple` and `base`.
* Complete raw Haskell bindings of `binaryen`, in the `binaryen` package.
* A serializable IR, roughly mapping to `binaryen` IR.

Currently working on:

* A custom linker that merges modules, handles relocations and supports tail-calls.

What comes next:

* An RTS written from scratch in native WebAssembly. Will implement
  enough primops & libc stubs for an MVP(Minimum Viable Product).
* A test suite for the generated WebAssembly code.

See [`ROADMAP.md`](ROADMAP.md) for a more detailed roadmap. The haddock documentation of the `master` branch is available [here](https://tweag.github.io/asterius/index.html).

## Building

Assumes x64 platform, tested on Linux and Windows.

`asterius` requires a build of recent `ghc-head` which uses `integer-simple` and disables tables-next-to-code. On Linux, you can either:

* `docker pull terrorjack/ghc-asterius`. This is a Nix-based build environment, with `stack` and `ghc-head` already installed. It's used for our CircleCI test suite.
* Build `nixpkgs.haskell.compiler.ghcAsterius`. The relevant Nix expressions can be found [here](https://github.com/TerrorJack/nixpkgs/tree/wip-ghc-asterius), they are not merged into upstream `nixpkgs` yet.

On Windows, a manually built `ghc-head` binary dist is available. It's already contained in [`stack.yaml`](stack.yaml), so for Windows users a plain `stack build` should work out of the box.

Extra dependencies:

* `cmake`/`make`/`g++`: For building in-tree [`binaryen`](https://github.com/WebAssembly/binaryen)
* `autoconf`: For booting `ghc-prim`/`base`
* `nodejs`: For running tests
* `stack`

Simply run `stack build asterius`. Set `MAKEFLAGS=-j8` to pass flags to `make` for parallel building of `binaryen`. Run `stack test asterius:ahc-boot` to test if booting works.

## Differences with [WebGHC](https://webghc.github.io/)

* Doesn't depend on Emscripten/LLVM. There is no plan to port the C runtime and support C libraries, at least for now.
* Windows is supported.

## Sponsors

&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;
[![Tweag I/O](https://www.tweag.io/img/tweag-small.png)](http://tweag.io)

Asterius is maintained by [Tweag I/O](http://tweag.io/).

Have questions? Need help? Tweet at
[@tweagio](http://twitter.com/tweagio).
