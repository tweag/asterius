# Draft semantics of concurrency and foreign calls.

Note: This document assumes that every function takes exactly one
argument. Just imagine that it's the last argument in a fully
saturated call.

## Foreign export asynchronous

Suppose that a Haskell function `f` is exported to JavaScript
asynchronously (which might be the default). When JavaScript calls the
exported function with argument `v`, it has the effect of performing
the IO action `⟦f⟧ v`, where the translation `⟦f⟧` is defined as
follows:

    ⟦f⟧ v = do
       p <- allocate new promise
       let run_f = case try (return $ f $ jsToHaskell v) of
                     Left exn -> p.fails (exnToJS exn)
                     Right a  -> p.succeeds (haskellToJS a)
       forkIO run_f
       return p -- returned to JavaScript

Not specified here is whether the scheduler is allowed to steal a few
cycles to run previously forked threads.

N.B. This is just a semantics. We certainly have the option of
implementing the entire action completely in the runtime system.

Not yet specified: What is the API by which JavaScript would call an
asynchronously exported Haskell function? Would it, for example, use
API functions to construct a Haskell closure, then evaluate it?

## Foreign import asynchronous

Suppose that a JavaScript  function `g` is imported asynchronously
(which might be the default). Let types `a` and `b` stand for two
unknown but fixed types. The JavaScript function expects an argument
of type `a` and returns a `Promise` that (if successful) eventually
delivers a value of type `b`. When a Haskell thunk of the form `g e`
is forced (evaluated), the machine performs the following monadic
action, the result of which is (eventually) written into the thunk.

    do let v = haskellToJS e  -- evaluates e, converts result to JavaScript
      p <- g v               -- call returns a `Promise`, "immediately"
      m <- newEmptyMVar
      ... juju to associate m with p ... -- RTS primitive?
      result <- takeMVar m
      case result of Left fails -> ... raise asynchronous exception ...
                      Right b -> return $ jsToHaskell v

## CPU sharing

Suppose GHC wishes to say politely to the JavaScript engine, "every so
often I would like to use the CPU for a bounded time."  It looks like
Haskell would need to add a message to the JavaScript message queue,
such that the function associated with that messages is "run Haskell
for N ticks."  Is the right API to call `setTimeout` with a delay of 0
seconds?

## Concurrency sketch

Let's suppose the state of a Haskell machine has these components:

  - `F` ("fuel") is the number of ticks a Haskell thread can execute
    before returning control to JavaScript. This component is present
    only when Haskell code is running.

  - `R` ("running") is either the currently running Haskell thread, or
    if no thread is currently running, it is • ("nothing")

  - `Q` ("run queue") is a collection of runnable threads.

  - `H` ("heap") is the Haskell heap, which may contain `MVar`s and
    threads that are blocked on them.

Components `R` and `H` are used linearly, so they can be stored in
global mutable state.

The machine will enjoy a set of labeled transitions such as are
described in Simon PJ's paper on the "Awkward Squad."  Call these the
"standard transitions."  (The awkward-squad machine state is a single
term, formed by the parallel composition of `R` with all the threads
of `Q` and all the MVars of `H`. The awkward squad doesn't care about
order, but we do.)  To specify the standard transitions, we could add
an additional clock that tells the machine when to switch the running
thread `R` out for a new thread from the queue. Or we could leave the
context switch nondeterministic, as it is in the awkward-squad paper.
Whatever seems useful.

Every state transition has the potential use to fuel. Fuel might
actually be implemented using an allocation clock, but for semantics
purposes, we can simply decrement fuel at each state transition, then
gate the standard transitions on the condition `F > 0`.

At a high level, every invocation of Haskell looks the same:
JavaScript starts the Haskell machine in a state `⟨F, •, Q, H⟩`, and
the Haskell machine makes repeated state transitions until it reaches
one of two stopping states:

  - `⟨F',•, [], H'⟩`: no Haskell threads are left to run

  - `⟨0, ̧R', Q', H'⟩`: fuel is exhausted, in which case the machine
    moves the currently running thread onto the run queue, reaching
    state `⟨0, ̧•, R':Q', H'⟩`

Once one of these states is reached, GHC's runtime system takes two
actions:

 1. It allocates a polite request for the CPU and puts that request on
    the JavaScript message queue, probably using `setTimeout` with a
    delay of 0 seconds.

 2. It returns control to JavaScript.
