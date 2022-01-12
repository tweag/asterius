# GHC RTS scheduler refactoring

All discussion in this document refers to the non-threaded RTS.

## Potential semantics

GHC relies on the scheduler to manage both concurrency and foreign calls.  Foreign calls are in play because most foreign calls are asynchronous, so implementing a foreign call requires support from the scheduler.  A [preliminary sketch of possible semantics](./semantics) can be found in file `semantics.md`.

## JavaScript user experience

I have `foo.hs`. I can compile to `foo.wasm` and `foo.js`. `foo.wasm`
is a binary artifact that needs to be shipped with `foo.js`, nothing
else you need to know about this file. `foo.js` conforms to some
JavaScript module standard and exports a JavaScript object. Say this
object is `foo`.

For each exported top-level Haskell function, `foo` contains a
corresponding async method. Consider the most common case `main :: IO
()`, then you can call `foo.main()`. For something like `fib :: Int ->
Int`, you can do `let r = await foo.fib(10)` and get the number result
in `r`. The arguments and result can be any JavaScript value, if the
Haskell type is `JSVal`.

Now, suppose we `await foo.main()`, and `main` finished successfully.
The RTS must remain alive, because:

- `main` might have forked other Haskell threads, those threads are
  expected to run in the background.
- `main` might have dynamically exported a Haskell function closure as
  a `JSFunction`. This `JSFunction` is passed into the outside
  JavaScript world, and it is expected to be called back some time in
  the future.

Notes regarding error handling: any unhandled Haskell exception is
converted to a JavaScript error. Likewise, any JavaScript error is
converted to a Haskell exception.

Notes regarding RTS startup: `foo` encapsulates some RTS context. That
context is automatically initialized no later than the first time you
call any method in `foo`.

Notes regarding RTS shutdown: not our concern yet. As long as the
browser tab is alive, the RTS context should be alive.

## Primer

ghc-devs thread: [Thoughts on async RTS
API?](https://mail.haskell.org/pipermail/ghc-devs/2021-December/020459.html)

ghc commentary:
[scheduler](https://gitlab.haskell.org/ghc/ghc/-/wikis/commentary/rts/scheduler)

## Consider a native case...

Suppose we'd like to run some Haskell computation from C (e.g. the `main`
function). After the RTS state is initialized, we need to:

1. If the Haskell function expects arguments, call the `rts_mk*`
   functions in
   [`RtsAPI.h`](https://gitlab.haskell.org/ghc/ghc/-/blob/master/rts/include/RtsAPI.h#L492)
   to convert C argument values to Haskell closures. Call `rts_apply`
   repeatedly to apply the Haskell function closure to argument
   closures, until we end up with a closure of Haskell type `IO a` or
   `a`, ready to be evaluated.
2. Call one of the eval functions in `RtsAPI.h`. The eval function
   creates a TSO(Thread State Object), representing the Haskell thread
   where the computation happens.
3. The eval function does some extra bookkeeping, then enters the
   scheduler loop.
4. The scheduler loop exits when the initial Haskell thread finishes.
   The thread return value and exit code is recorded.
5. The eval function retrieves the thread return value and exit code.
   We need to check whether the thread completed successfully, if so,
   we can call one of `rts_get*` functions in `RtsAPI.h` to convert
   the result Haskell closure to C value.

The key logic is in the
[`schedule`](https://gitlab.haskell.org/ghc/ghc/-/blob/master/rts/Schedule.c#L178)
function which implements the scheduler loop. The implementation is
quite complex, for now we only need to keep in mind:

- In each iteration, the Haskell thread being run is not necessarily
  the initial thread we created to kick off evaluation. New threads
  may get forked and executed, but the loop exits only when the
  initial thread finishes!
- Threads may block due to a variety of reasons, they will be
  suspended and resumed as needed. It may be possible that all live
  threads are blocked, RTS will attempt to make progress by collecting
  file descriptors related to blocking I/O and do a `select()` call,
  to ensure I/O can proceed for at least one file descriptor.

## The problem

Suppose we'd like to call an async JavaScript function and get the
result in Haskell:

```haskell
foreign import javascript safe "fetch($1)" js_fetch :: JSRequest -> IO JSResponse
```

In Haskell, when `js_fetch` returns, the actual `fetch()` call should
have already resolved; if it rejected, then an exception should be
raised in Haskell.

Now, the main thread calls `js_fetch` at some point, no other threads
involved. According to previous section, the current call stack is
something like:

```
main -> rts_evalLazyIO -> scheduleWaitThread -> schedule -> fetch
```

The Haskell code does a `fetch()` call (or it arranges the RTS to
perform one). `fetch()` will immediately return a `Promise` handle.
Now what? What do we do with this `Promise` thing? More importantly,
the scheduler loop can't make any progress! The Haskell thread is
blocked, suspended, the run queue is empty, the RTS scheduler only
knows about posix blocking read/write, so it doesn't know how to
handle this situation.

After `fetch()` returns, the call stack is:

```
main -> rts_evalLazyIO -> scheduleWaitThread -> schedule
```

Remember the
["run-to-completion"](https://developer.mozilla.org/en-US/docs/Web/JavaScript/EventLoop)
principle of the JavaScript concurrency model! We're currently inside
some JavaScript/WebAssembly function, which counts as a single tick in
the entire event loop. The functions we're running right now must run
to completion and return, only after that, the `fetch()` result can
become available.

And also remember how the WebAssembly/JavaScript interop works: you
can only import synchronous JavaScript functions, and export
WebAssembly functions as synchronous JavaScript functions. Every C
function in RTS that we cross-compile to WebAssembly is also
synchronous, no magic blocking or preemptive context switch will ever
take place!

## What we need

All the scheduler-related synchronous C functions in RTS, be it
`rts_eval*` or `schedule`, they only return when the initial Haskell
thread completes. We must teach these functions to also return when
the thread blocks, at least when blocking reason is beyond
conventional posix read/write.

Here's how things should look like after the scheduler is refactored:

1. There are async flavours of scheduler functions. When they return,
   the Haskell thread may have completed, or may have been blocked due
   to some reason. In that case, the returned blocking info will
   contain at least one file descriptor or `Promise` related to
   blocking, and also the blocked thread ids.
2. When we do async JavaScript calls, we attach resolve/reject
   handlers to the returned `Promise`. These handlers will resume the
   entire RTS and carry-on Haskell computation.
3. Since any Haskell thread may perform async JavaScript call, all
   Haskell functions are exported as async JavaScript functions. A
   `Promise` is returned immediately, but it's resolved/rejected in
   the future, when the corresponding Haskell thread runs to
   completion.


## Potential milestones

### RTS: integrating foreign event loops

**Draft**: 
The RTS scheduler is synchronous. If you call `rts_eval*` to enter the
scheduler and do some evaluation, it'll only return when the relevant
Haskell thread is completed or killed. This model doesn't work if we
want to be able to call async foreign functions without blocking the
entire RTS. The root of this problem: the scheduler loop has no
knowledge about foreign event loops.

_Status_: we have looked into this, and based on our experience in
Asterius, the implementation plan is as follows:

- Add CPS-style async versions of `rts_eval*` RTS API functions.
  Original sync versions continue to work, but panics with a
  reasonable error message when unsupported foreign blocking event
  occurs.

- The scheduler loop is broken down into "ticks". Each tick runs to
  the point when some Haskell computation finishes or blocks, much
  like a single iteration in the original scheduler loop. The
  scheduler ticks can be plugged into a foreign event loop, so Haskell
  evaluation fully interleaves with other foreign computation.

_GHC support required:_

- Restructuring of the current scheduler.

### RTS: make usage of `select`/`poll` optional

In the current non-threaded RTS, when there are no immediately
runnable Haskell threads, a `select()` call will be performed on all
the file descriptors related to blocking. The call returns when I/O is
possible for at least one file descriptor, therefore some Haskell
thread blocked on I/O can be resumed.

This may work for us when we target pure `wasm32-wasi` instead of the
browser. The WASI standard defines a `poll_oneoff` syscall, and
`wasi-libc` implements `select()`/`poll()` using this syscall.

However, this doesn't work well with JavaScript runtime (or any
foreign event loop in general). `poll()` calls are blocking calls, so
they can block the entire event loop, hang the browser tab and prevent
"real work" (e.g. network requests) from proceeding.

_Status_: we have looked into this, and there are roughly two possible
approaches:

- Use the binaryen "asyncify" wasm rewriting pass to instrument the
  linked wasm module, to implement the blocking behavior of
  `poll_oneoff` without actually blocking the entire event loop. Easy
  to implement, but it's a very ugly hack that also comes with penalty
  in code size and performance.
- Restructure the scheduler, so that for non-threaded RTS, each
  scheduler tick will not attempt to do a blocking `poll()` call at
  all. The higher-level caller of scheduler ticks will be in charge of
  collecting blocking I/O events and handling them.


_GHC support required:_

- Same as previous subsection
