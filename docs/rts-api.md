## Invoking RTS API in JavaScript

For the brave souls who prefer to play with raw pointers instead of syntactic sugar, it's possible to invoke RTS API directly in JavaScript. This grants us the ability to:

* Allocate memory, create and inspect Haskell closures on the heap.
* Trigger Haskell evaluation, then retrieve the results back into JavaScript.
* Use raw Cmm symbols to summon any function, not limited to the "foreign exported" ones.

Here is a simple example. Suppose we have a `Main.fact` function:

```Haskell
fact :: Int -> Int
fact 0 = 1
fact n = n * fact (n - 1)
```

The first step is ensuring `fact` is actually contained in the final WebAssembly binary produced by `ahc-link`. `ahc-link` performs aggressive dead-code elimination (or more precisely, live-code discovery) by starting from a set of "root symbols" (usually `Main_main_closure` which corresponds to `Main.main`), repeatedly traversing ASTs and including any discovered symbols. So if `Main.main` does not have a transitive dependency on `fact`, `fact` won't be included into the binary. In order to include `fact`, either use it in some way in `main`, or supply `--extra-root-symbol=Main_fact_closure` flag to `ahc-link` when compiling.

The next step is locating the pointer of `fact`. The "asterius instance" type we mentioned before contains two "symbol map" fields: `staticsSymbolMap` maps static data symbols to linear memory absolute addresses, and `functionSymbolMap` maps function symbols to WebAssembly function table indices. In this case, we can use `i.staticsSymbolMap.Main_fact_closure` as the pointer value of `Main_fact_closure`. For a Haskell top-level function, there're also pointers to the info table/entry function, but we don't need those two in this example.

Since we'd like to call `fact`, we need to apply it to an argument, build a thunk representing the result, then evaluate the thunk to WHNF and retrieve the result. Assuming we're passing `--asterius-instance-callback=i=>{ ... }` to `ahc-link`, in the callback body, we can use RTS API like this:

```JavaScript
i.wasmInstance.exports.hs_init();
const argument = i.wasmInstance.exports.rts_mkInt(5);
const thunk = i.wasmInstance.exports.rts_apply(i.staticsSymbolMap.Main_fact_closure, argument);
const tid = i.wasmInstance.exports.rts_eval(thunk);
console.log(i.wasmInstance.exports.rts_getInt(i.wasmInstance.exports.getTSOret(tid)));
```

A line-by-line explanation follows:

* As usual, the first step is calling `hs_init` to initialize the runtime.
* Assuming we'd like to calculate `fact 5`, we need to build an `Int` object which value is `5`. We can't directly pass the JavaScript `5`, instead we should call `rts_mkInt`, which properly allocates a heap object and sets up the info pointer of an `Int` value. When we need to pass a value of basic type (e.g. `Int`, `StablePtr`, etc), we should always call `rts_mk*` and use the returned pointers to the allocated heap object.
* Then we can apply `fact` to `5` by using `rts_apply`. It builds a thunk without triggering evaluation. If we are dealing with a curried multiple-arguments function, we should chain `rts_apply` repeatedly until we get a thunk representing the final result.
* Finally, we call `rts_eval`, which enters the runtime and perform all the evaluation for us. There are different types of evaluation functions:
  * `rts_eval` evaluates a thunk of type `a` to WHNF.
  * `rts_evalIO` evaluates the result of `IO a` to WHNF.
  * `rts_evalLazyIO` evaluates `IO a`, without forcing the result to WHNF. It is also the default evaluator used by the runtime to run `Main.main`.
  * `rts_evalStableIO` evaluates the result of `StablePtr (IO a)` to WHNF, then return the result as `StablePtr a`.
* All `rts_eval*` functions initiate a new Haskell thread for evaluation, and they return a thread ID. The thread ID is useful for inspecting whether or not evaluation succeeded and what the result is.
* If we need to retrieve the result back to JavaScript, we must pick an evaluator function which forces the result to WHNF. The `rts_get*` functions assume the objects are evaluated and won't trigger evaluation.
* Assuming we stored the thread ID to `tid`, we can use `getTSOret(tid)` to retrieve the result. The result is always a pointer to the Haskell heap, so additionally we need to use `rts_getInt` to retrieve the unboxed `Int` content to JavaScript.

Most users probably don't need to use RTS API manually, since the `foreign import`/`export` syntactic sugar and the `makeHaskellCallback` interface should be sufficient for typical use cases of Haskell/JavaScript interaction. Though it won't hurt to know what is hidden beneath the syntactic sugar, `foreign import`/`export` is implemented by automatically generating stub WebAssembly functions which calls RTS API for you.
