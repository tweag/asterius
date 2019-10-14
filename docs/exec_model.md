# Execution model

## JavaScript execution model

JS engines maintain a set of tasks. Among them, the runnable tasks are in a
queue (macro-task queue). Each task of the queue is executed until its
completion without any kind of preemption. A long running task may "block" the
execution of other tasks. This is especially noticeable in a Web browser when
the DOM rendering task and the UI event handler tasks are blocked.

![JS execution model](js_exec_model.svg)

Each task is in fact composed of a queue of "micro-tasks". Initially there is a
single micro-task in the queue. This task can queue other micro-tasks with
``queueMicrotask(task)``. These micro-tasks are executed in FIFO order. When the
micro-task queue is empty, the macro-task has completed.

**Promises**

[Promises](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Promise)
are JS synchronization objects. When they settle they can either be fulfilled or
rejected.

```
p = new Promise((resolve,reject) => whatever)
```

``whatever`` is executed synchronously. ``resolve`` and ``reject`` are functions to set the Promise value.

```
p.then(r => doSomething)
p.catch(r => doSomething)
```

``doSomething`` is always executed asynchronously.

In a micro-task we detect if ``p`` is settled: if it is, we execute ``doSomething``,
otherwise ``doSomething`` will be executed in a future macro-task (when ``p`` gets
settled)

```
b = p.then(f).then(g)
```

is equivalent to:

```
a = p.then(f)
b = a.then(g)
```
so we queue several micro-tasks in order just like before. If we only have
settled Promises, the execution is very fast as no macro-task creation/switching
occurs.

``Promise.all(xs)`` waits for all the promises to settle.
``Promise.race(xs)`` returns the first settled Promise in xs (eiter resolved or rejected)

With ES2017 we can somewhat replace Promises with
[await/async](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Operators/await):

```
async function (x,y) {
   try {
      let a = await p
      let b = await (f a)
      ...
   } catch(errhandler)
}
```

* ``await`` can only be used in an ``async`` function
* ``await p`` creates a delimited continuation ``c`` so that ``p.then(c)``

**Workers**

JS supports some kind of multi-threading via
[Workers](https://developer.mozilla.org/en-US/docs/Web/API/Web_Workers_API). A
Worker has its own task queues and execution engine. Each worker has its own JS
context and Workers are executed concurrently (e.g. one OS thread per Worker).

Communication between Workers is done via message passing. It is also possible
to use shared memory via a
[SharedBufferArray](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/SharedArrayBuffer).
Note: some JS engines have disabled it for security reasons. At the time of
writing, it is available in Node and the latest versions of Chrome.

**Wasm threads**

There is a
[proposal](https://github.com/WebAssembly/threads/blob/master/proposals/threads/Overview.md)
to add support for threads in WebAssembly reusing Worker and SharedBufferArray
but it isn't implemented yet.

## Haskell execution model

Haskell threads are represented by a Thread State Object (TSO). Each TSO has its
own Stack.

In the native RTS, TSOs are executed by Capabilities (virtual CPUs). Each
Capability has a run queue of TSOs and also maintains lists of TSOs blocked for
different reasons. Capabilities are executed by Tasks, where a Task is a wrapper
for an OS thread. Task specific data are stored in thread-local storage (TLS). A
Task owns a single Capability at once.

The Haskell Heap is shared between all the Haskell threads. TSOs themselves
are objects in the heap.

**Scheduling**

TSOs can be preempted when they call into the runtime system. It can be:

* when they
  [yield](https://www.stackage.org/haddock/lts-14.5/base-4.12.0.0/Control-Concurrent.html#v:yield) (cooperative scheduling)
* when they call into blocking functions (``takeMVar``, etc.)
* when they call into FFI functions
* when they don't have enough heap space to allocate

The runtime system may force a TSO preemption by faking a reduction of the
heap space to 0. The preemption only happens when the TSO tries to allocate
though (hopefully often enough).

## Executing Haskell code with JS engines

We somehow need to map Haskell execution model on JavaScript one.

### Step 0: no multi-threading

The global shared memory is a WebAssembly.Memory object (see
[memory](memory.md)). Currently it can't be shared between several JavaScript
contexts.

Asterius only has a single Capability (as in the non-threaded native RTS).

Currently Asterius [doesn't support
multi-threading](https://github.com/tweag/asterius/issues/268). A single TSO is
executed synchronously until it finishes, blocking any other macro-task.

In practice: ``rts_eval*`` functions in ``rts.Exports.mjs`` create a new thread
(using ``createThread`` function in Asterius.Builtins) and execute it
synchronously with ``scheduleWaitThread`` defined in Asterius.Builtins.

### Step 1: cooperative multi-threading

We could introduce a run queue of TSOs. Each time a TSO calls back into the
scheduler we can schedule another TSO.

We can postpone the execution of the next TSO with
``setTimeout(executeNextTSO,0)`` in order to let the JS engine execute other
macro-tasks in between. E.g. JSFFI using ``setTimeout``, browser events, etc.

### Step 2 (optional): cooperative multi-threading + Worker

Execute the whole Asterius instance in a Worker. We can execute TSOs as fast as
possible without yielding to the JS engine event loop.

Provide stubs using message passing to:

* Call Haskell functions (executed in the Worker) from the JS main context.
* Call JSFFI JS codes (executed in the JS main context) from the Worker.
