
# NoFib: Haskell Benchmark Suite (GEORGE's NOTES)

This folder contains a selection of tests taken from [GHC's `nofib` testsuite](https://gitlab.haskell.org/ghc/nofib).

## Changelog

Differences compared to the original `nofib`:

* We do not use any symlinks (we replicate files instead). Easier to reproduce.
* The `gc`, `parallel`, and `smp` subdirectories are omitted.
* Every test lives at the same level (arbitrary nesting complicates things for no benefit).
* We do not use a Makefile-based system to build the tests.
* Removed test `shootout/reverse-complement` (too big inputs and outputs, used c program to generate them)

## Executing the Tests
To remove all material from previous executions, just run:
```bash
python3 script.py cleanup #
```

To run the tests, type the following:
```bash
python3 script.py <MODE>
```
where `<MODE>` is one of `fast`, `norm`, or `slow`. This should create a `TIMES.txt` file, whose content is in the following CSV format:
```
category, testname, compiler, mode, duration (in seconds)
```
For example, it contains entries like the following:
```
imaginary,primes,ghc,fast,0.23458600044250488
imaginary,primes,ahc,fast,1.422548770904541
imaginary,tak,ghc,fast,0.08010053634643555
imaginary,tak,ahc,fast,1.0395824909210205
...
```

## Current Failures (Details)

```
shootout/pidigits     (unknown reason)
shootout/k-nucleotide (unknown reason)
```

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
```

