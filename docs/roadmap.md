# Project status & roadmap

## Overview

The Asterius project has come a long way and some examples with complex
dependencies already work. It's still less mature than GHCJS though; see the next
section for details.

In general, it's hard to give ETA for "production readiness", since improvements
are continuous, and we haven't collected enough use cases from seed users yet.
For more insight into what comes next for this project, we list our quarterly
roadmap here.

Besides the goals in each quarter, we also do regular maintenance like
dependency upgrades and bugfixes. We also work on related projects (mainly
[`haskell-binaryen`](https://github.com/tweag/haskell-binaryen) and
[`inline-js`](https://github.com/tweag/inline-js)) to ensure they are kept in
sync and also useful to regular Haskell developers.

## What works now

- Almost all GHC language features (TH support is partial, cross-splice state
  persistence doesn't work yet).
- The pure parts in standard libraries and other packages. IO is achieved via
  rts primitives or user-defined JavaScript imports.
- Importing JavaScript expressions via the `foreign import javascript` syntax.
  First-class garbage collected `JSVal` type in Haskell land.
- Preliminary copying GC, managing both Haskell heap objects and JavaScript
  references.
- Cabal support. Use `ahc-cabal` to compile libraries and executables. Support
  for custom `Setup.hs` is limited.
- Marshaling between Haskell/JavaScript types based on `aeson`.
- Calling Haskell functions from JavaScript via the `foreign export javascript`
  syntax. Haskell closures can be passed between the Haskell/JavaScript boundary
  via `StablePtr`.
- Invoking RTS API on the JavaScript side to manipulate Haskell closures and
  trigger evaluation.
- A linker which performs aggressive dead-code elimination, based on symbol
  reachability.
- A debugger which checks invalid memory access and outputs memory loads/stores
  and control flow transfers.
- Complete [`binaryen`](https://github.com/WebAssembly/binaryen) raw bindings,
  plus a monadic EDSL to construct WebAssembly code directly in Haskell.
- [`wasm-toolkit`](https://github.com/tweag/asterius/tree/master/wasm-toolkit):
  a Haskell library to handle WebAssembly code, which already powers binary code
  generation.
- Besides WebAssembly MVP and `BigInt`, there are no special requirements on
  the underlying JavaScript engine at the moment.

## What may stop one from using Asterius right now

- Lack of JavaScriptCore/Safari support, due to incomplete JavaScript `BigInt`
  support at the moment.
- Runtime bugs. The generated code comes with a complex hand-written runtime
  which is still buggy at times. The situation is expected to improve once we're
  able to work with an IR more high-level than Cmm and shave off the current
  hand-written garbage collector; see the 2020 Q3 section for more details.
- GHCJS projects aren't supported out of the box. Major incompatibilities
  include:
  - Word sizes differ. Asterius is still 64-bit based at the moment.
  - JSFFI syntax and semantics differ. Asterius uses `Promise`-based async JSFFI
    and GHCJS uses callbacks.
  - Cabal handles GHCJS and Asterius differently.
- Lack of Nix support.
- Lack of GHCi support.
- TH support is not 100% complete; certain TH API which require preserving state
  across splices (e.g. `getQ`/`putQ`) don't work yet.
- Cabal tests and benchmarks can't be run out of the box.
- Custom `Setup.hs` support is limited. If it has `setup-deps` outside GHC boot
  libs, it won't work.
- Lack of profiling support for generated code.
- Excessive memory usage when linking large programs.

## Quarterly roadmap

### 2020 Q3

Work in 2020 Q3 is focused on:

- Introducing C/C++ toolchain support. The first step is to introduce libc in
  the generated wasm code, and use libc functionality to replace certain runtime
  functionality (e.g. memory management). Once we're confident our runtime and
  generated code is compatible with libc, we'll look into building & linking C
  source files in Haskell packages.
- Research on a high-level variant of Cmm which abstracts away closure
  representation and can be efficiently mapped to platforms providing host
  garbage collection (e.g. wasm-gc, JavaScript, JVM). This will enable us to
  avoid relying on a hand-written custom garbage collector and improve the
  runtime reliability significantly.
