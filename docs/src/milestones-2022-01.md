# Project Milestones, January 2022 edition

The goals for Asterius are described on the page [WebAssembly goals](https://gitlab.haskell.org/ghc/ghc/-/wikis/WebAssembly-goals) on the [GHC Wiki](https://gitlab.haskell.org/ghc/ghc/-/wikis/WebAssembly-goals).  This document describes some milestones on the path to those goals.

## Getting to JavaScript-free functionality

Although JavaScript interoperation is the big use case, much of the support needed for WebAssembly is independent of JavaScript.

### Codegen: New back end

A new back end will have to be defined in a way that fits into GHC's
existing structure.

_GHC support required:_

- ~~Either a new value constructor for `Backend` or (likely) changes to
  the `NcgImpl` record~~ Make the `Backend` type abstract, add a new value constructor for it.

### Codegen: handling arbitrary control-flow graphs

WebAssembly lacks `goto` and provides only structured control flow:
loops, blocks, `if` statements, and multilevel `continue`/`break`. A
Cmm control-flow graph must be converted to this structured control.

_Status_: A [prototype](https://github.com/tweag/stg-translation-prototype/blob/main/GHC/Wasm/ControlFlow/OfCmm.hs)
has been implemented and tested, but the prototype works only on
reducible control-flow graphs. A transformation from irreducible to
reducible CFGs has yet to be implemented.

_GHC support required:_

- Dominator analysis on `CmmGraph`

### Codegen: fit linking information into standard object files

The Asterius prototype emits object files that are represented in a
custom format. This format contains _ad hoc_ information that can be
handled only by a custom linker. The information currently stored in
custom object files must either be expressed using standard object
files that conform to C/C++ toolchain convention, or it must be
eliminated.

_Status_: All information currently emitted by the Asterius prototype
can be expressed using standard object files, with one exception:
JSFFI records. We plan to turn these records into standard data
segments whose symbols will be reachable from related Haskell
functions. Such segments can be handled by a standard C/C++ linker.
The data segments will be consumed by the JavaScript adjunct to GHC's
run-time system, which will use them to reconstruct imported and
exported functions.

_GHC support required:_

- None

### Codegen: implement WebAssembly IR and binary encoder

Rather than attempt to prettyprint WebAssembly directly from Cmm, the
WebAssembly back end will first translate Cmm to an internal
representation of a WebAssembly module, tentatively to be called
`WasmModule`. A `WasmModule` can be serialized to the standard
WebAssembly binary format.

A preliminary design might look like this:

- A `WasmModule` contains sections
- A section may contain functions, memory segments or other metadata
- A function body is control flow (`WasmStmt ...`)
- Control flow may contain straight-line code
- Straight-line code may be a tree structure or may be a sequence of
  Wasm instructions

_Status_: Except for that `WasmStmt` fragment, which contains the
WebAssembly control-flow constructs, the internal representation has
yet to be defined.
And we have yet to reach consensus on whether we wish to be able to
emit both textual and binary WebAssembly, or whether we prefer to emit
only binary WebAssembly and to rely on an external disassembler to
produce a more readable representation.  (External assemblers are
apparently not good enough to be able to rely on emitting only a
textual representation.)


_GHC support required:_

- None

### Codegen: implement Cmm to WebAssembly IR codegen

We need a translator from `CmmGroup` to `WasmModule`. Our prototype
relooper translates `CmmGraph` to `WasmStmt ...`, and the other parts
of the translation should mostly be a 1-to-1 mapping. Some Cmm
features can be translated in more than one way:

- _Global registers._ We can use the in-memory register table as in
  unregisterised mode, or one WebAssembly global for each global
  register, or use WebAssembly multi-value feature to carry the
  registers around. Start with WebAssembly globals first, easy to
  implement, should be reasonably faster than memory load/store.

- _Cmm tail calls._ We can use WebAssembly experimental tail calls
  feature, or do trampolining by making each Cmm function return its
  jump target. Since WebAssembly tail calls is not widely implemented
  in engines yet, start with trampolining.

_Status_: Not started, but given the rich experience with the Asterius
prototype, no difficulties are anticipated.

_GHC support required:_

- None

### Build system

The build system has to be altered to select the proper C code for the
WebAssembly target. We're hoping for the following:

- The build system can build and package the run-time system
  standalone.

- The build system can easily cross-compile from a POSIX host to the
  Wasm target.

- A developer can instruct the build system to choose Wasm-compatible
  features selectively to build and test on a POSIX platform
  (so-called "feature vector").

Meeting these goals will require both conditional build rules _and_
CPP macros for code specific to `wasm32-wasi`.

_Status_: Not yet begun.

_GHC support required:_

- Coordination with the cross-compilation team (Sylvain Henry, John
  Ericson)

### RTS: avoid `mmap`

The run-time storage manager uses `mmap` and `munmap` to allocate and
free `MBlock`s. But `mmap` and `munmap` aren't available on the WASI
platform, so we need to use standard libc allocation routines instead.

_Status_: we implemented the patch, tested with WebAssembly, i386 and
x64-without-large-address-space.

_GHC support required:_

- New directory `rts/wasi` to go alongside `rts/posix` and
  `rts/win32`.

- Altered logic in `rts/rts.cabal.in` and elsewhere to use conditional
  compilation to select `OSMem.c` from the `rts/wasi` directory.

### RTS: replace the timer used in the scheduler

The run-time system currently uses a timer to know when to deliver a
Haskell Execution Context (virtual CPU) to another Haskell thread. But
the timer is implemented using pthreads and POSIX signals, which are
not available on WebAssembly---so it has to go. We'll need some other
method for deciding when to switch contexts.

This change will remove dependencies on pthreads and on a POSIX signal
(VTALRM).

_Status_: We have patched the run-time system to disable that timer,
and we have tested the patch on POSIX. In this patch, the scheduler
does a context switch at every heap-block allocation (as in the [`-C0`
RTS
flag](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/using-concurrent.html#rts-flag--C%20%E2%9F%A8s%E2%9F%A9)).
Yet to be done: determine a viable long-term strategy for deciding
when to context switch.

_GHC support required:_

- Patches to scheduler, of a detailed nature to be specified later

### RTS: replace other uses of POSIX signals

The run-time system depends on the signals API in various ways: it can
handle certain OS signals, and it can even support setting Haskell
functions as signal handlers. Such functionality, which inherently
depends on signals, must be made conditional on the target platform.

There is already a `RTS_USER_SIGNALS` CPP macro that guards some
signal logic, but not all. To make signals truly optional, more work
is needed.

_Status_: In progress.

_GHC support required:_

- Not yet known

### RTS: port libffi to WebAssembly

`libffi` is required for dynamic exports to C. It's technically
possible to port `libffi` to either pure WebAssembly or
WebAssembly+JavaScript.

_Status_: Not yet implemented.

_GHC support required:_

- Likely none.

## Milestones along the way to full JavaScript interoperability

_(The audience for this section is primarily the Asterius
implementation team, but there are a few things that ought to be
communicated to other GHC implementors.)_

### RTS for JSFFI: representing and garbage-collecting foreign references

When Haskell interoperates with JavaScript, Haskell objects need to be
able to keep JavaScript objects alive and vice versa, even though they
live on different heaps. Similarly, JavaScript needs to be able to
reclaim JavaScript objects once there are no more references to them.

We propose to extend GHC with a new primitive type `JSVal#`, whose
closure payload is a single word. The JavaScript adjunct uses this
word to index into an internal table. After each major garbage
collection, the collector notifies the JavaScript adjunct of all live
`JSVal#` closures. The adjunct uses this report to drop its references
to JavaScript objects that cannot be reached from the Haskell heap.

_Status_: Not yet implemented.

_GHC support required:_

- Build-system support for the JavaScript adjunct to the RTS

- New primitive type `JSVal#`

- Patch to the garbage collector to report live `JSVal#` closures.

### RTS: API/semantics for scheduling and JavaScript foreign calls

Write down and document whatever API is needed for calls across the Haskell/JavaScript boundary and for sharing the single CPU among both Haskell threads and JavaScript's event loop.  Ideal documentation would include a small-step operational semantics.

_Status_: [Work in progress](semantics.md)


_GHC support required:_

- Coordinate with GHCJS team (unclear at what stage)

### RTS: Scheduler issues

GHC's scheduler will need to be altered to support an event-driven model of concurrency. The details are [work in progress](scheduler.md).
