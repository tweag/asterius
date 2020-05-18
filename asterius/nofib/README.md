# NoFib: Haskell Benchmark Suite (GEORGE's NOTES)

This folder contains a selection of tests taken from [GHC's `nofib` testsuite](https://gitlab.haskell.org/ghc/nofib).

## Changelog

Differences compared to the original `nofib`:

* We do not use any symlinks (we replicate files instead). Easier to reproduce.
* The `gc`, `parallel`, and `smp` subdirectories are omitted.
* Every test lives at the same level (arbitrary nesting complicates things for no benefit).
* We do not use a Makefile-based system to build the tests.
* TODO: Mention all differences here.

## Executing the Tests

```bash
make distclean                             # Remove any obsolete files to ensure recompilation of all tests.
time ./run.sh FAST ghc 2>&1 | tee log-ghc  # run each test in FAST mode, using $(which ghc)
make clean                                 # remove object files but leave .stdout and .stderr
time ./run.sh FAST ahc 2>&1 | tee log-ahc  # run each test in FAST mode, using $(which ahc-link)
./stdoutdiff.sh 2>&1 | tee log-comp        # Compare (TODO: subject to change)
```

## Current Failures

```
1   | real/eff-S        : I gave up on this one
1   | shootout/pidigits : I gave up on this one
----+------------------------------------------
111 | TOTAL RUN
    +------------------------------------------
    | 100 PASSED
    +------------------------------------------
    |  11 FAILED
    |   * real/eff-VS			| Wrong Output (empty)
    |   * real/maillist			| Wrong Output (empty)
    |   * real/pic			| JS Stacktrace        <== possibly relevant: https://github.com/nodejs/node/issues/3171
    |   * real/scs			| JS Stacktrace
    |   * real/symalg			| Wrong Output (incomplete)
    |   * shootout/binary-trees		| Wrong Output (incomplete)
    |   * shootout/fannkuch-redux	| JS Stacktrace
    |   * shootout/k-nucleotide		| Wrong Output (empty)
    |   * shootout/n-body		| Wrong Output (empty)
    |   * spectral/cichelli		| Wrong Output (empty)
    |   * spectral/primetest		| Wrong Output (empty)
----+------------------------------------------
```

## Current Failures (Details)

### JavaScript heap out of memory
```
real/pic
  FATAL ERROR: Ineffective mark-compacts near heap limit Allocation failed - JavaScript heap out of memory
   1: 0xa295e0 node::Abort() [/usr/bin/node]
   2: 0x9782df node::FatalError(char const*, char const*) [/usr/bin/node]
   3: 0xb99c2e v8::Utils::ReportOOMFailure(v8::internal::Isolate*, char const*, bool) [/usr/bin/node]
   4: 0xb99fa7 v8::internal::V8::FatalProcessOutOfMemory(v8::internal::Isolate*, char const*, bool) [/usr/bin/node]
   5: 0xd3a3b5  [/usr/bin/node]
   6: 0xd3ad5b v8::internal::Heap::RecomputeLimits(v8::internal::GarbageCollector) [/usr/bin/node]
   7: 0xd48b52 v8::internal::Heap::PerformGarbageCollection(v8::internal::GarbageCollector, v8::GCCallbackFlags) [/usr/bin/node]
   8: 0xd499a5 v8::internal::Heap::CollectGarbage(v8::internal::AllocationSpace, v8::internal::GarbageCollectionReason, v8::GCCallbackFlags) [/usr/bin/node]
   9: 0xd4c36c v8::internal::Heap::AllocateRawWithRetryOrFailSlowPath(int, v8::internal::AllocationType, v8::internal::AllocationOrigin, v8::internal::AllocationAlignment) [/usr/bin/node]
  10: 0xd119ca v8::internal::Factory::AllocateRaw(int, v8::internal::AllocationType, v8::internal::AllocationAlignment) [/usr/bin/node]
  11: 0xd0f264 v8::internal::FactoryBase<v8::internal::Factory>::AllocateRawWithImmortalMap(int, v8::internal::AllocationType, v8::internal::Map, v8::internal::AllocationAlignment) [/usr/bin/node]
  12: 0xd170ca v8::internal::Factory::NewBigInt(int, v8::internal::AllocationType) [/usr/bin/node]
  13: 0xe59fdf v8::internal::MutableBigInt::NewFromDouble(v8::internal::Isolate*, double) [/usr/bin/node]
  14: 0xe5a7d1 v8::internal::BigInt::FromNumber(v8::internal::Isolate*, v8::internal::Handle<v8::internal::Object>) [/usr/bin/node]
  15: 0xc0f402  [/usr/bin/node]
  16: 0xc10656 v8::internal::Builtin_BigIntConstructor(int, unsigned long*, v8::internal::Isolate*) [/usr/bin/node]
  17: 0x13a5b79  [/usr/bin/node]

real/scs
  FATAL ERROR: Ineffective mark-compacts near heap limit Allocation failed - JavaScript heap out of memory
   1: 0xa295e0 node::Abort() [/usr/bin/node]
   2: 0x9782df node::FatalError(char const*, char const*) [/usr/bin/node]
   3: 0xb99c2e v8::Utils::ReportOOMFailure(v8::internal::Isolate*, char const*, bool) [/usr/bin/node]
   4: 0xb99fa7 v8::internal::V8::FatalProcessOutOfMemory(v8::internal::Isolate*, char const*, bool) [/usr/bin/node]
   5: 0xd3a3b5  [/usr/bin/node]
   6: 0xd3ad5b v8::internal::Heap::RecomputeLimits(v8::internal::GarbageCollector) [/usr/bin/node]
   7: 0xd48b52 v8::internal::Heap::PerformGarbageCollection(v8::internal::GarbageCollector, v8::GCCallbackFlags) [/usr/bin/node]
   8: 0xd499a5 v8::internal::Heap::CollectGarbage(v8::internal::AllocationSpace, v8::internal::GarbageCollectionReason, v8::GCCallbackFlags) [/usr/bin/node]
   9: 0xd4c36c v8::internal::Heap::AllocateRawWithRetryOrFailSlowPath(int, v8::internal::AllocationType, v8::internal::AllocationOrigin, v8::internal::AllocationAlignment) [/usr/bin/node]
  10: 0xd119ca v8::internal::Factory::AllocateRaw(int, v8::internal::AllocationType, v8::internal::AllocationAlignment) [/usr/bin/node]
  11: 0xd0f264 v8::internal::FactoryBase<v8::internal::Factory>::AllocateRawWithImmortalMap(int, v8::internal::AllocationType, v8::internal::Map, v8::internal::AllocationAlignment) [/usr/bin/node]
  12: 0xd153f9 v8::internal::Factory::NewForeign(unsigned long) [/usr/bin/node]
  13: 0xf20ea1 v8::internal::FrameArray::AppendWasmFrame(v8::internal::Handle<v8::internal::FrameArray>, v8::internal::Handle<v8::internal::WasmInstanceObject>, int, v8::internal::wasm::WasmCode*, int, int) [/usr/bin/node]
  14: 0xcdf413  [/usr/bin/node]
  15: 0xcdffe7 v8::internal::Isolate::CaptureAndSetSimpleStackTrace(v8::internal::Handle<v8::internal::JSReceiver>, v8::internal::FrameSkipMode, v8::internal::Handle<v8::internal::Object>) [/usr/bin/node]
  16: 0xcf11e1 v8::internal::ErrorUtils::Construct(v8::internal::Isolate*, v8::internal::Handle<v8::internal::JSFunction>, v8::internal::Handle<v8::internal::Object>, v8::internal::Handle<v8::internal::Object>, v8::internal::FrameSkipMode, v8::internal::Handle<v8::internal::Object>, v8::internal::ErrorUtils::StackTraceCollection) [/usr/bin/node]
  17: 0xc2b43a  [/usr/bin/node]
  18: 0xc2c806 v8::internal::Builtin_ErrorConstructor(int, unsigned long*, v8::internal::Isolate*) [/usr/bin/node]
  19: 0x13a5b79  [/usr/bin/node]

shootout/fannkuch-redux
  FATAL ERROR: Ineffective mark-compacts near heap limit Allocation failed - JavaScript heap out of memory
   1: 0xa295e0 node::Abort() [/usr/bin/node]
   2: 0x9782df node::FatalError(char const*, char const*) [/usr/bin/node]
   3: 0xb99c2e v8::Utils::ReportOOMFailure(v8::internal::Isolate*, char const*, bool) [/usr/bin/node]
   4: 0xb99fa7 v8::internal::V8::FatalProcessOutOfMemory(v8::internal::Isolate*, char const*, bool) [/usr/bin/node]
   5: 0xd3a3b5  [/usr/bin/node]
   6: 0xd3ad5b v8::internal::Heap::RecomputeLimits(v8::internal::GarbageCollector) [/usr/bin/node]
   7: 0xd48b52 v8::internal::Heap::PerformGarbageCollection(v8::internal::GarbageCollector, v8::GCCallbackFlags) [/usr/bin/node]
   8: 0xd499a5 v8::internal::Heap::CollectGarbage(v8::internal::AllocationSpace, v8::internal::GarbageCollectionReason, v8::GCCallbackFlags) [/usr/bin/node]
   9: 0xd4c36c v8::internal::Heap::AllocateRawWithRetryOrFailSlowPath(int, v8::internal::AllocationType, v8::internal::AllocationOrigin, v8::internal::AllocationAlignment) [/usr/bin/node]
  10: 0xd1ba0b v8::internal::Factory::NewFillerObject(int, bool, v8::internal::AllocationType, v8::internal::AllocationOrigin) [/usr/bin/node]
  11: 0x104bdaf v8::internal::Runtime_AllocateInYoungGeneration(int, unsigned long*, v8::internal::Isolate*) [/usr/bin/node]
  12: 0x13a5a99  [/usr/bin/node]

------------------------------------------------------------------------------+
```

### Ongoing Investigation

```
real/symalg
  NO VISIBLE ERROR.

shootout/k-nucleotide
  NO VISIBLE ERROR.

spectral/cichelli
  NO VISIBLE ERROR.

spectral/primetest
  NO VISIBLE ERROR.

real/eff-VS:
  file:///home/skull/tweag/asterius-alt/asterius/nofib/real/eff-VS/rts.memory.mjs:118
      this.memory.grow(n);
```

### Missing functions (bugs)
```
real/maillist:
  Main: JSException "RuntimeError: unreachable
      at wasm-function[714]:0x2251f
      at wasm-function[3235]:0xa8707
      at wasm-function[3236]:0xa8734
      at Scheduler.tick (file:///home/skull/tweag/asterius-alt/asterius/nofib/real/maillist/rts.scheduler.mjs:346:22)
      at Immediate.<anonymous> (file:///home/skull/tweag/asterius-alt/asterius/nofib/real/maillist/rts.scheduler.mjs:381:29)
      at processImmediate (internal/timers.js:456:21)"

shootout/binary-trees
  Main: JSException "RuntimeError: unreachable
      at wasm-function[108]:0x6e91
      at wasm-function[4070]:0xd77de
      at wasm-function[4071]:0xd780b
      at Scheduler.tick (file:///home/skull/tweag/asterius-alt/asterius/nofib/shootout/binary-trees/rts.scheduler.mjs:346:22)
      at Immediate.<anonymous> (file:///home/skull/tweag/asterius-alt/asterius/nofib/shootout/binary-trees/rts.scheduler.mjs:381:29)
      at processImmediate (internal/timers.js:456:21)"

shootout/n-body
  Main: JSException "RuntimeError: unreachable
      at wasm-function[41]:0x3fe7
      at wasm-function[4102]:0xd96f5
      at wasm-function[4103]:0xd9722
      at Scheduler.tick (file:///home/skull/tweag/asterius-alt/asterius/nofib/shootout/n-body/rts.scheduler.mjs:346:22)
      at Immediate.<anonymous> (file:///home/skull/tweag/asterius-alt/asterius/nofib/shootout/n-body/rts.scheduler.mjs:381:29)
      at processImmediate (internal/timers.js:456:21)"
```

## Other Notes

* Removed test `shootout/reverse-complement` (too big inputs and outputs, used c program to generate them)

