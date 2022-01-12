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

### 2021 Q3

For the past months before this update, I took a break from the Asterius project
and worked on a client project instead. There's a saying "less is more", and I
believe my absense in this project for a few months is beneficial in multiple
ways:

- I gained a lot more nix-related knowledge.
- Purging the short-term memory on the project and coming back, this gives me
  some insight on the difficulties of onboarding new contributors.
- After all, it was a great mental relief to work on something which I was
  definitely not a bottleneck of the whole project.

Before I took the break, Asterius was stuck with a very complex & ad-hoc build
system, and it was based on ghc-8.8. The most production-ready major version of
ghc is ghc-8.10 today. Therefore, Q3 goals and roadmap has been adjusted
accordingly:

- Upgrade Asterius to use ghc-8.10. The upgrade procedure should be principled &
  documented, so someone else can repeat this when Asterius upgrades to ghc-9.2
  in the future.
- Use cabal & nix as the primary build system.

What has been achieved so far:

- There is a new ghc fork dedicated for asterius at
  https://github.com/tweag/ghc-asterius. It's based on `ghc-8.10` branch, the
  previous asterius-specific patches have all been ported, and I implemented
  nix-based logic to generate cabal-buildable ghc api packages to be used by
  Asterius, replacing the previous ad-hoc python script.
- There is a WIP branch of ghc-8.10 & nix support at
  https://github.com/tweag/asterius/pull/860. Most build errors in the host
  compiler have been fixed, and the booting logic will be fixed next.
- A wasi-sdk/wasi-libc fork is also maintained in the tweag namespace. It's
  possible to configure our ghc fork with `wasm32-unknown-wasi` triple now, so
  that's a good start for future work of proper transition of Asterius to a
  wasi32 backend of ghc.

Remaining work of Q3 will be wrapping up #860 and merging it to `master`.

Beyond Q3, the overall plan is also guided by the "less is more" principle: to
reduce code rather than to add, leveraging upstream logic whenever possible,
while still maintaing and even improving end-user experience. Many hacks were
needed in the past due to various reasons, and after all the lessons learned
along the way, there are many things that should be shaved off:

- The hacks related to 64-bit virtual address space. Reusing host GHC API which
  targets 64-bit platform for Asterius was the easiest way to get the MVP
  working, but given we have much better knowledge about how cross-compiling in
  ghc works, these hacks needs to go away.
- Custom object format and linking logic. This was required since Asterius
  needed to record a lot of Haskell-specific info in the object files: JSFFI
  imports/exports, static pointer table, etc. However, with runtime support,
  these custom info can all be replaced by vanilla data sections in the wasm or
  llvm bitcode object files.
- Following the entry above, most of the existing wasm codegen logic. It looks
  possible to leverage the llvm codegen, only adding specific patches to support
  features like JSFFI.
- Most of the existing JavaScript runtime. They will be gradually replaced by
  cross-compiled ghc rts for the wasi32 target, component after component. The
  ultimate goal is to support generating self-contained JavaScript-less wasm
  modules which work in runtimes beyond browsers/nodejs (that's why we stick to
  `wasi-sdk` instead of `emscripten` in the first place).

### 2021 Q1

In 2020 Q4 we mainly delivered:

- Use standalone stage-1 GHC API packages and support building Asterius using
  vanilla GHC.
- Remove numerous hacks and simplify the codebase, e.g.:
  - Make `ahc` a proper GHC frontend exe, support `ahc -c` on non-Haskell
    sources
  - Use vanilla archives and get rid of custom `ahc-ar`
- Refactor things incompatible with 32-bit pointer convention, e.g.:
  - Proper heap layout for `JSVal#` closures
  - Remove higher 32-bit data/function address tags

In 2021 Q1, the primary goals are:

- Finish transition to 32-bit code generation.
- Improve C/C++ support, including support for `integer-gmp` and `cbits` in
  common packages.

The plan to achieving above goals:

- Audit the current code generator & runtime and remove everything incompatible
  with 32-bit pointer convention.
- For the time being, favor simplicity/robustness over performance. Some
  previous optimizations may need to be reverted temporarily to simplify the
  codebase and reduce the refactoring overhead.
- Use `wasi-sdk` as the C toolchain to configure the stage-1 GHC and finish the
  transition.

A longer term goal beyond Q1 is upstreaming Asterius as a proper wasm backend of
GHC. We need to play well with `wasi-sdk` for this to happen, so another thing
we're working on in Q1 is: refactor the linker infrastructure to make it
LLVM-compliant, which means managing non-standard entities (e.g. static
pointers, JSFFI imports/exports) in a standard-compliant way.

### 2020 Q4

In 2020 Q3 we mainly delivered:

- PIC(Position Independent Code) support. We worked on PIC since in the
  beginning, we thought it was a prerequisite of C/C++ support. Turned out it's
  not, but still PIC will be useful in the future when we implement dynamic
  linker and ghci support.
- Initial C/C++ support, using `wasi-sdk` to compile C/C++ sources. Right now
  this doesn't work Cabal yet, so the C/C++ sources need to be manually added to
  `asterius/libc` to be compiled and linked. We already replaced quite some
  legacy runtime shims with actual C code (e.g. `cbits` in `bytestring`/`text`),
  and more will come in the future.

Proper C/C++ support requires Asterius to be a proper `wasm32`-targetting cross
GHC which is configured to use `wasi-sdk` as the underlying toolchain. The
immediate benefits are:

- Get rid of various hacks due to word size mismatch in the code emitted by
  Asterius and `wasi-sdk`. Some packages (e.g. `integer-gmp`) are incompatible
  with these hacks.
- Implement proper Cabal integration and support `cbits` in user packages.
- Improve code size and runtime performance, getting rid of the `i64`/`i32`
  pointer casting everywhere.
- Get rid of `BigInt` usage in the JavaScript runtime, and support running
  generated code in Safari.

Thus the goal of 2020 Q4 is finishing the 32-bit cross GHC transition. The steps
to achieve this is roughly:

- Detangle the host/wasm GHC API usage. Asterius will shift away from using
  `ghc` of the host GHC and instead use its own stage-1 GHC API packages.
- Fix various issues when configuring GHC to target `wasm32-wasi` and using
  `wasi-sdk` as the toolchain.
- Refactor the code generator and the runtime to work with the new 32-bit
  pointer convention.

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
