## WebAssembly as a Haskell compilation target

There are a few issues to address when compiling Cmm to WebAssembly.

### Implementing Haskell Stack/Heap

The Haskell runtime maintains a TSO(Thread State Object) for each Haskell thread, and each TSO contains a separate stack for the STG machine. The WebAssembly platform has its own "stack" concept though; the execution of WebAssembly is based on a stack machine model, where instructions consume operands on the stack and push new values onto it.

We use the linear memory to simulate Haskell stack/heap. Popping/pushing the Haskell stack only involves loading/storing on the linear memory. Heap allocation only involves bumping the heap pointer. Running out of space will trigger a WebAssembly trap, instead of doing GC.

All discussions in the documentation use the term "stack" for the Haskell stack, unless explicitly stated otherwise.

### Implementing STG machine registers

The Haskell runtime makes use of "virtual registers" like Sp, Hp or R1 to implement the STG machine. The NCG tries to map some of the virtual registers to real registers when generating assembly code. However, WebAssembly doesn't have language constructs that map to real registers, so all virtual registers are implemented as local variables of the interpreter function.

### Handling control flow

WebAssembly currently enforces structured control flow, which prohibits arbitrary branching. Also, explicit tail calls are missing.

The Cmm control flow mainly involves two forms of branching: in-function or cross-function. Each function consists of a map from `hoopl` labels to basic blocks and an entry label. Branching happens at the end of each basic block.

In-function branching is relatively easier to handle. `binaryen` provides a "relooper" which can recover WebAssembly instructions with structured control flow from a control-flow graph. For each `CmmGraph` we invoke the relooper to handle branching between basic blocks.

Cross-function branching (`CmmCall`) is tricky. WebAssembly lacks explicit tail calls, and the relooper can't be easily used in this case since there's a computed goto, and potential targets include all Cmm blocks involved in linking. There are multiple possible ways to handle this situation:

* Collect all Cmm blocks into one function, additionally add a "dispatcher" block. All `CmmCall`s save the callee to a register and branch to the "dispatcher" block, and the "dispatcher" uses `br_table` or a binary decision tree to branch to the entry block of callee.
* One WebAssembly function for one `CmmProc`, and upon `CmmCall` the function returns the function id of callee. A mini-interpreter function at the top level repeatedly invoke the functions using `call_indirect`. This approach is actually used by the unregisterised mode of `ghc`.

We're still investigating the best way. The first approach probably produces the fastest code, at the cost of no dynamic linking (not a scheduled feature anyway) and potential slowdown when linking large Haskell programs (unless an O(n) relooping algorithm is implemented).

### Relocations

When producing a WebAssembly binary, we need to map `CLabel`s to the precise linear memory locations for `CmmStatics` or the precise table ids for `CmmProc`s. They are unknown when compiling individual modules, so `binaryen` is invoked only when linking, and during compiling we only convert `CLabel`s to some serializable representation.

It's also worth noting that currently only `wasm32` is implemented, but we are running 64-bit `ghc`, so extra care need to be taken when computing memory locations.
