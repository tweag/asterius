
# GEORGE's NOTES

```
make distclean
time ./run.sh FAST ghc 2>&1 | tee log-ghc
make clean
time ./run.sh FAST ahc 2>&1 | tee log-ahc
./stdoutdiff.sh 2>&1 | tee log-comp
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

ERRORS FOUND:

real/eff-VS:
  file:///home/skull/tweag/asterius-alt/asterius/nofib/real/eff-VS/rts.memory.mjs:118
      this.memory.grow(n);

real/maillist:
  Main: JSException "RuntimeError: unreachable
      at wasm-function[714]:0x2251f
      at wasm-function[3235]:0xa8707
      at wasm-function[3236]:0xa8734
      at Scheduler.tick (file:///home/skull/tweag/asterius-alt/asterius/nofib/real/maillist/rts.scheduler.mjs:346:22)
      at Immediate.<anonymous> (file:///home/skull/tweag/asterius-alt/asterius/nofib/real/maillist/rts.scheduler.mjs:381:29)
      at processImmediate (internal/timers.js:456:21)"

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

real/symalg
  NO VISIBLE ERROR.

shootout/binary-trees
  Main: JSException "RuntimeError: unreachable
      at wasm-function[108]:0x6e91
      at wasm-function[4070]:0xd77de
      at wasm-function[4071]:0xd780b
      at Scheduler.tick (file:///home/skull/tweag/asterius-alt/asterius/nofib/shootout/binary-trees/rts.scheduler.mjs:346:22)
      at Immediate.<anonymous> (file:///home/skull/tweag/asterius-alt/asterius/nofib/shootout/binary-trees/rts.scheduler.mjs:381:29)
      at processImmediate (internal/timers.js:456:21)"

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

shootout/k-nucleotide
  NO VISIBLE ERROR.

shootout/n-body
  Main: JSException "RuntimeError: unreachable
      at wasm-function[41]:0x3fe7
      at wasm-function[4102]:0xd96f5
      at wasm-function[4103]:0xd9722
      at Scheduler.tick (file:///home/skull/tweag/asterius-alt/asterius/nofib/shootout/n-body/rts.scheduler.mjs:346:22)
      at Immediate.<anonymous> (file:///home/skull/tweag/asterius-alt/asterius/nofib/shootout/n-body/rts.scheduler.mjs:381:29)
      at processImmediate (internal/timers.js:456:21)"

spectral/cichelli
  NO VISIBLE ERROR.

spectral/primetest
  NO VISIBLE ERROR.

------------------------------------------------------------------------------+

* Checked spectral.
* Checked smp.
* Checked shootout.
* Checked real.
* Checked parallel.
* Checked imaginary.
* Checked gc ==> Seems to be a lot of trouble (see its Makefile) / TODO.

* REMOVED TEST: shootout/reverse-complement
  (too big inputs and outputs, used c program to generate them)


/home/skull/tweag/asterius/asterius/nofib/spectral/hartel/Makefile included:
  SRC_MKDEPENDHS_OPTS=-fglasgow-exts

TODO: In GHC, spectral/secretary was actually disabled (because it required
`random`). See if we wish to keep it or not.

TODO: smp seems to ignore some tests. Taken from its Makefile:
  SUBDIRS = sieve callback001 callback002 threads001 threads003 threads006 threads007 chan
  # Not a good benchmark, can go really slowly for random reasons: threads002
  OTHER_SUBDIRS = stm001 stm002 systolic tchan theads002 threads004 threads005

TODO: Decide what should be done with /home/skull/tweag/asterius/asterius/nofib/smp/smpbench.sh

TODO: Taken from the makefile of shootout:
  To run the benchmarks under the official settings, pass `mode=slow` to make.
  You might also want to pass `EXTRA_HC_OPTS="-fllvm"`

TODO: Note in real:
  #cacheprof causes very nondeterministic allocation
  OTHER_SUBDIRS = cacheprof

TODO: parallel seems to ignore some tests. Taken from its Makefile:
  SUBDIRS = parfib partree sumeuler matmult ray gray prsa mandel queens coins blackscholes minimax nbody transclos
  OTHER_SUBDIRS = cfd dcbm linsolv nbody partak quicksort threadfib warshall

Problematic Makefiles
~~~~~~~~~~~~~~~~~~~~~
spectral/awards


~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


# NoFib: Haskell Benchmark Suite

This is the root directory of the "NoFib Haskell benchmark suite". It
should be part of a GHC source tree, that is the 'nofib' directory
should be at the same level in the tree as 'compiler' and 'libraries'.
This makes sure that NoFib picks up the stage 2 compiler from the
surrounding GHC source tree.

You can also clone this repository in isolation, in which case it will
pick `$(which ghc)` or whatever the `HC` environment variable is set to.

Additional information can also be found on
[NoFib's wiki page](https://ghc.haskell.org/trac/ghc/wiki/Building/RunningNoFib).

There's also a `easy.sh` helper script, which as name implies, is
automated and easy way to run `nofib`.
See the section at the end of README for its usage.

## Using

<details>
  <summary>Git symlink support for Windows machines</summary>

  NoFib uses a few symlinks here and there to share code between benchmarks.
  Git for Windows has symlinks support for some time now, but
  [it may not be enabled by default](https://stackoverflow.com/a/42137273/388010).
  You will notice strange `make boot` failures if it's not enabled for you.

  Make sure you follow the instructions in the link to enable symlink support,
  possibly as simple as through `git config core.symlinks true` or cloning with
  `git clone -c core.symlinks=true <URL>`.
</details>

Install [`cabal-install-2.4`](https://www.haskell.org/cabal/download.html) or later.

Then, to run the tests, execute:

```
$ make clean # or git clean -fxd, it's faster
$ # Generates input files for the benchmarks and builds compilation
$ # dependencies for make (ghc -M)
$ make boot
$ # Builds the benchmarks and runs them $NoFibRuns (default: 5) times
$ make
```

This will put the results in the file `nofib-log`. You can pass extra
options to a nofib run using the `EXTRA_HC_OPTS` variable like this:

```
$ make clean
$ make boot
$ make EXTRA_HC_OPTS="-fllvm"
```

**Note:** to get all the results, you have to `clean` and `boot` between
separate `nofib` runs.

To compare the results of multiple runs, save the output in a logfile
and use the program in `./nofib-analyse/nofib-analyse`, for example:

```
...
$ make 2>&1 | tee nofib-log-6.4.2
...
$ make 2>&1 | tee nofib-log-6.6
$ nofib-analyse nofib-log-6.4.2 nofib-log-6.6 | less
```

to generate a comparison of the runs in captured in `nofib-log-6.4.2`
and `nofib-log-6.6`. When making comparisons, be careful to ensure
that the things that changed between the builds are only the things
that you _wanted_ to change. There are lots of variables: machine,
GHC version, GCC version, C libraries, static vs. dynamic GMP library,
build options, run options, and probably lots more. To be on the safe
side, make both runs on the same unloaded machine.

## Modes

Each benchmark is runnable in three different time `mode`s:

- `fast`: 0.1-0.2s
- `norm`: 1-2s
- `slow`: 5-10s

You can control which mode to run by setting an additional `mode` variable for
`make`. The default is `mode=norm`. Example for `mode=fast`:

```
$ make clean
$ make boot mode=fast
$ make mode=fast
```

Note that the `mode`s set in `make boot` and `make` need to agree. Otherwise you
will get output errors, because `make boot` will generate input files for a
different `mode`. A more DRY way to control the `mode` would be

```
$ make clean
$ export mode=fast
$ make boot
$ make
```

As CPU architectures advance, the above running times may drift and
occasionally, all benchmarks will need adjustments.

Be aware that `nofib-analyse` will ignore the result if it falls below 0.2s.
This is the default of its `-i` option, which is of course incompatible with
`mode=fast`. In that case, you should just set `-i` as appropriate, even
deactivate it with `-i 0`.

## Boot vs. benchmarked GHC

The `nofib-analyse` utility is compiled with `BOOT_HC` compiler,
which may be different then the GHC under the benchmark.

You can control which GHC you benchmark with `HC` variable

```
$ make clean
$ make boot HC=ghc-head
$ make HC=ghc-head 2>&1 | tee nofib-log-ghc-head
```

## Configuration

There are some options you might want to tweak; search for nofib in
`../mk/config.mk`, and override settings in `../mk/build.mk` as usual.

## Extra Metrics: Valgrind

To get instruction counts, memory reads/writes, and "cache misses",
you'll need to get hold of Cachegrind, which is part of
[Valgrind](http://valgrind.org).

You can then pass `-cachegrind` as `EXTRA_RUNTEST_OPTS`. Counting
instructions slows down execution by a factor of ~30. But it's
a deterministic metric, so you can combine it with `NoFibRuns=1`:

```
$ (make EXTRA_RUNTEST_OPTS="-cachegrind" NoFibRuns=1) 2>&1 | tee nofib-log
```

Optionally combine this with `mode=fast`, see [Modes](#modes).

## Extra Packages

Some benchmarks aren't run by default and require extra packages are
installed for the GHC compiler being tested. These packages include:
 * stm - for smp benchmarks

## Adding benchmarks

If you add a benchmark try to set the problem sizes for
fast/normal/slow reasonably. [Modes](#modes) lists the recommended brackets for
each mode.

### Benchmark Categories

So you have a benchmark to submit but don't know in which subfolder to put it? Here's some
advice on the intended semantics of each category.

#### Single threaded benchmarks

These are run when you just type `make`. Their semantics is explained in
[the Nofib paper](https://link.springer.com/chapter/10.1007%2F978-1-4471-3215-8_17)
(You can find a .ps online, thanks to @bgamari. Alternatively grep for
'Spectral' in docs/paper/paper.verb).

- `imaginary`: Mostly toy benchmarks, solving puzzles like n-queens.
- `spectral`: Algorithmic kernels, like FFT. If you want to add a benchmark of a
  library, this most certainly the place to put it.
- `real`: Actual applications, with a command-line interface and all. Because of
  the large dependency footprint of today's applications, these have become
  rather aged.
- `shootout`: Benchmarks from
  [the benchmarks game](https://benchmarksgame-team.pages.debian.net/benchmarksgame/),
  formerly known as "language shootout".

Most of the benchmarks are quite old and aren't really written in way one would
write high-performance Haskell code today (e.g., use of `String`, lists,
redefining own list combinators that don't take part in list fusion, rare use of
strictness annotations or unboxed data), so new benchmarks for the `real` and
`spectral` in brackets in particular are always welcome!

#### Other categories

Other than the default single-threaded categories above, there are the
following (SG: I'm guessing here, have never run them):

- `gc`: Run by `make -C gc` (though you'll probably have to edit the Makefile to
  your specific config). Select benchmarks from `spectral` and `real`, plus a
  few more (Careful, these have not been touched by #15999/!5, see the next
  subsection). Testdrives different GC configs, apparently.
- `smp`: Microbenchmarks for the `-threaded` runtime, measuring scheduler
  performance on concurrent and STM-heavy code.

### Stability wrt. GC paramerisations

Additionally, pay attention that your benchmarks are stable wrt. different
GC paramerisations, so that small changes in allocation don't lead to big,
unexplicable jumps in performance. See #15999 for details. Also make sure
that you run the benchmark with the default GC settings, as enlarging Gen 0 or
Gen 1 heaps just amplifies the problem.

As a rule of thumb on how to ensure this: Make sure that your benchmark doesn't
just build up one big data and consume it in a final step, but rather that the
working set grows and shrinks (e.g. is approximately constant) over the whole
run of the benchmark. You can ensure this by iterating your main logic $n times
(how often depends on your program, but in the ball park of 100-1000).
You can test stability by plotting productivity curves for your `fast` settings
with the `prod.py` script attached to #15999.

If in doubt, ask Sebastian Graf for help.

## easy.sh

```
./easy.sh - easy nofib

Usage: ./easy.sh [ -m mode ] /path/to/baseline/ghc /path/to/new/ghc"

GHC paths can point to the root of the GHC repository,
if it's build with Hadrian.

Available options:
  -m MODE  nofib mode: fast norm slow

This script caches the results using the sha256 of ghc executable.
Remove these files, if you want to rerun the benchmark.
```
