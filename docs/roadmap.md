# Project roadmap

This document maintains a list of milestones along with their planned features. Some notations:

* M0, M1, .. indicates Milestone 0, Milestone 1, etc. The numbers grow monotonically.
* P0, P1, .. incicates Priority 0, Priority 1, etc. The lesser the number, the more significant is the feature.
* A P0 feature blocks a milestone if it is not shipped.
* A P1 feature can delay a milestone if it can be shipped with a reasonable delay.
* A P2 feature can be re-scheduled or modified to prevent blocking a milestone.

## M1

Approximate delivery time: Apr 15, 2018

The purpose of M1 is to ship an MVP(Minimum Viable Product).

### P0 features

* Can compile a standalone Haskell executable involving simple pure computations plus a few `IO` routines like `putStrLn`. The output will be a linked WebAssembly module with HTML/JavaScript loader, and the effect can be demostrated via outputing to a browser console.
* If involved with unsupported primops, foreign calls, etc, a compile-time error instead of a run-time error will be given.
* The wasm rts allocates linear memory only once during startup, and only provides a bump allocator. No GC is implemented. A stack/heap overflow causes the runtime to exit immediately and report error in JavaScript.

### P1 features

* A separate `binaryen` package to provide raw bindings to the `binaryen` project.
* A separate `wasm-toolkit` package to provide a Haskellish IR wrapping `binaryen`, also providing simple utilities like simulating tail-calls, linking multiple modules, eliminating dead code, etc.

### P2 features

* A test suite which uses Node.js/Headless Chrome to test the generated WebAssembly modules.
* A separate `inline-javascript` package to provide quasi-quoters for evaluating JavaScript embedded in Haskell, useful for the test suite.
* A build system based on nix/bazel instead of docker for developers on Linux/Mac.
* Fix Windows support, set up AppVeyor for testing the project itself/building the custom ghc bindist required for the project.

## M2

Approximate delivery time: Jun 30, 2018

At this point the project status changes from pre-alpha to alpha, meaning it's actually possible to make one or two useful gadgets out of this project.

### P0 features

* The wasm rts performs GC. There will be a naive hand-written GC which prevents stack/heap overflow in the long run, but the GC pause may be quite noticable at this point.
* An FFI mechanism, more feature-rich than the current WebAssembly import/export mechanism. It supports keeping handles of arbitrary JavaScript values in the wasm rts and invoking JavaScript functions with those values. Calling back from the JavaScript world is not supported yet.

### P1 features

* Support more primops, preferrably everything in `GHC.Prim` but excluding the following parts (they are planned for milestones beyond M2):
    * Threading
    * Weak references
    * StableName/StablePtr
    * Compact regions
    * Sparks
    * Bytecode
    * SIMD
    * Prefetch
* More real-world demos, since with FFI it's already possible to work with DOM API.

### P2 features

* A separate `ghc-toolkit` package to provide a framework for compiling from Haskell to anything. Users will only need to be familiar with various GHC IR types, and don't need to bother with details like `Cabal` integration, frontend plugins, booting, etc.
* Integration with existing JavaScript ecosystem, possibly something like an `npm` package or a `webpack` plugin.
* A wiki for potential contributors.
* A blog post about yet another great Tweag I/O project.
