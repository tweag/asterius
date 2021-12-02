# Migrating GHC's Run-Time  System to WebAssembly

Asterius's prototype uses a custom run-time system hand-written in
JavaScript.  But in the long term, maintaining a separate, parallel
run-time system is not in the cards.  GHC's native run-time system
must be refactored so it can run in the WebAssembly environment.

The main barrier to running GHC's native run-time system is that the
WebAssembly machine does not offer a POSIX API.  That is, WebAssembly
lacks the *mechanisms* currently used by GHC's run-time system.
And in some cases, WebAssembly not only doesn't have the API calls; it
doesn't even provide the services.  For example, it does not provide a
way to interrupt a running program in the way that a POSIX signal (or
a hardware interrupt) does.  

Trying to replace existing POSIX mechanisms is not the game we want to play.
Instead, we'd like to identify *features* in the run-time system that
are currently implemented using POSIX system calls, but that on
WebAssembly can be implemented in other ways.  For example, in a POSIX
process, memory can be obtained with `mmap`, but in WebAssembly, it is
obtained by the `memory.grow` instruction---or if C libraries are
loaded, by `malloc`.

## Potential features identified

Discussions so far have suggested these features:

  - *Address-space management.*  The run-time system may need to
    acquire more address space from the underlying platform, or to
    return some.
  
      * Use `mmap` and `munmap` (POSIX)

      * Use `malloc` and `free` (WebAssembly using WASI SDK or similar)

  - *Context switching.*  Something needs to signal to the scheduler
    that it's time to switch the Haskell Execution Context from one
    Haskell thread to another.

      * Use a timer and signals (POSIX)

      * Switch at every opportunity (all platforms)

      * Switch every N opportunities (all platforms)

      * Look at a clock to decide whether to switch

  - *Response to user interrupts.*  When a user hits Ctl-C or
    equivalent, how does the run-time system find out?

      * Signal handler catches the event and sets a bit (POSIX)

      * Other mechanism?  Or perhaps WebAssembly cannot support this feature.

  - *Interpreter support.* Supporting GHCi requires `libffi`, which is
    not available on WebAssembly.  It should be possible to build the
    run-time system without support for GHCi.

## Role of features in development and testing

Once features have been identified, GHC's build system can be extended
so that the implementation of each feature can be selected by a flag
at compiler-configuration time.  Collectively, these configuration
flags constitute a *feature vector.* By choosing an appropriate
feature vector, it will be easy to build a hybrid run-time system that
uses a mix of features that do and do not require POSIX mechanisms---and this
run-time system can be used to test non-POSIX mechanisms one at a
time, on a congenial platform like Linux.

Using a feature vector will also have advantages for debugging.
If a validation test exposes a fault, we can use automation (like
QuickCheck) to find a _minimal_ set of non-POSIX mechanisms features that
trigger the fault.  Debugging a run-time system can be very
time-consuming, and using automation to isolate buggy features should help.

Configuration mechanisms for features will be influenced by the
following considerations:

  - Conditional compilation will be controlled by `cpp` macros, _not_
    by conditional build rules within Hadrian.  As an example of such
    a symbol that is already used, see `RTS_LINKER_USE_MMAP`.

  - GHC Central endorses conditional compilation on a file
    granularity.  Thus, each distinct implementation of a feature gets
    its own file or set of files, and then the relevant file is
    compiled by means of a conditional `#include`.  This method is
    more clunky than (say) putting the conditional building into the
    rules for compilation and linking, but it does keep the build
    rules simple.

    For a helpful example of this method, look at the implementation
    in `rts/posix/Ticker.c`.

  - `cpp` macros are likely to be set or unset using Cabal flags.
    Some example flags can be found in `rts/rts.cabal.in`.  In those
    examples, the `default` values are currently set by Autoconf,
    which does macro substitution on the `rts.cabal.in` file.
    But once the Make build is retired, those macros and the
    substitutions will go away.  Instead, Hadrian will slurp up the
    definitions and will set the flags.  (Hadrian will have a list of
    the flags it needs to know about.)

    For an example of setting flags, see
    `Hadrian/src/Settings/Packages.hs`.  To understand the whole
    picture, look for how `libnuma` is handled.

    We should feel free to add new Cabal flags that collectively
    determine the feature vector.

Yet to be determined: how to select and build multiple feature vectors
for differential testing.


## Mechanisms identified that may need replacing

We are currently (October/November 2021) working through GHC's native run-time
system to find dependencies on POSIX mechanisms, which we then hope to
group into features as defined above.  As of 18 November, our laundry
list looks like this:

  - Signals

  - `libffi`[^libffi]

  - I/O multiplexing with `select` (supported by WASI, but possibly
    not compatible with POSIX)

  - Dynamic linking (e.g., `dlopen`)

  - BSD sockets

[^libffi]: This library is used to help with foreign calls.  Its
implementation is deeply platform-dependent.  Some dependencies can be
eliminated by eliminating interpreter support.  Otherwise we may have to port a
subset of libffi to WebAssembly.
