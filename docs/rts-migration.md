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

      * Use `malloc` and `free` (WebAssembly using Emscripten)

  - *Context switching.*  Something needs to signal to the scheduler
    that it's time to switch the Haskell Execution Context from one
    Haskell thread to another.

      * Use a timer and signals (POSIX)

      * Switch at every opportunity (all platforms)

      * Switch every N opportunities (all platforms)

  - *Response to user interrupts.*  When a user hits Ctl-C or
    equivalent, how does the run-time system find out?

      * Signal handler catches the event and sets a bit (POSIX)

      * Other mechanism?

## Role of features in development and testing

Once features have been identified, GHC's build system can be extended
so that the implementation of each feature can be selected by a flag
at compiler-configuration time.  Collectively, these configuration
flags constitute a *feature vector.* By choosing an appropriate
feature vector, it will be easy to build a hybrid run-time system that
uses a mix of POSIX features and WebAssembly features---and this
run-time system can be used to test WebAssembly features one at a
time, on a congenial platform like Linux.

Using a feature vector will also have advantages for debugging.
If a validation test exposes a fault, we can use automation (like
QuickCheck) to find a _minimal_ set of WebAssembly features that
trigger the fault.  Debugging a run-time system can be very
time-consuming, and using automation to isolate buggy features should help.

## Mechanisms identified that need replacing

We are currently (October/November 2021) working through GHC's native run-time
system to find dependencies on POSIX mechanisms, which we then hope to
group into features as defined above.  As of 28 October, our laundry
list looks like this:

  - Signals

