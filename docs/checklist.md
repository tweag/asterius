## Checklist

This page maintains a list of upcoming tasks for the project, each task with a brief explanation, estimation of difficulty/time and connections with other tasks. Ideally this should be called **Roadmap** instead of **Checklist**, but placing accurate milestones has proven to be hard.

### Towards a TodoMVC example

The tasks listed in this section are all necessary ones to achieve a more "real-world" browser example like TodoMVC. They are currently being worked on.

#### Implement `foreign export javascript`

We already can call JavaScript from Haskell via `foreign import javascript`, for JavaScript to call into Haskell, we need to implement `foreign export javascript`. The exported Haskell functions will be available as WebAssembly exported functions, callable in JavaScript land.

Requirements:

* Implement `StablePtr`, so that Haskell closures can be safely passed between Haskell/JavaScript boundary without being garbage collected. (done, see `stableptr` unit test)
* Implement `RtsAPI`, so that JavaScript code can create Haskell closures, trigger evaluation and inspect results. (done, see `rtsapi` unit test)
* Add `StablePtr` to JSFFI basic types. (done, see `jsffi` unit test)
* Implement `foreign export javascript` syntax, add necessary logic in `JSFFI`/`Resolve` (done, see `jsffi` unit test)

#### Improve `.wasm`/`.js` generation

Currently, given a home module, `ahc-link` outputs a `.wasm` and a `.js` wrapper which runs in Node.js. We will need the whole thing to run in browser though.

Requirements:

* Add logic in `ahc-link` to generate browser-friendly code
* Make the test suite run via a headless browser, and properly retrieve results from the browser back to Haskell

### Next important tasks

The following tasks are somewhat less important, but still necessary for end-user experience. They will be processed once the previous goal is accomplished.

#### Improve Haskell/JavaScript marshalling

Most JavaScript types will appear as opaque `JSRef` in Haskell land, but for some types that appear very often (e.g. strings and arrays), we wish they can be marshalled from/to their Haskell equivalents (e.g. lists) smoothly. Without this, even implementing a `putStr` will be troublesome because we must either send individual `Char`s to a TTY-device in JavaScript, or manually squeeze the string into a buffer in Haskell heap first.

Requirements:

* Recognize `String`/`[]` as special JSFFI basic types, add marshalling logic from/to JavaScript strings/arrays. (in progress)

When we support `aeson` in the future, it may even be possible to marshal between `Value`s and JavaScript objects directly.

#### Support `bytestring`

`bytestring` is a critical component in the Haskell ecosystem, we must support it regardless of what filthy hacks are deployed. At least non-`Internal` modules need to be supported.

Requirements:

* Add `bytestring` to boot libs.
* Implement `Weak#`, since `ByteString` needs finalizers
* Implement WebAssembly shims in `Builtins` for required C functions.

#### Utilize GHC renamer/typechecker in JSFFI

Currently, the JSFFI thing works with parsed AST because it's less likely to mess up after rewriting. As a consequence:

* `JSRef` only works as a magic identifier. It's not in any actual Haskell module
* No `newtypes` for JSFFI basic types, since we recognize types by `RdrName` only

We need to move JSFFI processing to the phase of renamer or typechecker.

#### Support JavaScript promises in JSFFI

The `foreign import javascript` syntax currently assumes the JavaScript computation is synchronous. We need to support asynchronous JavaScript computation, by adding a `foreign import javascript safe` construct, and assume the JavaScript computation returns a `Promise`. Upon calling such a function, the scheduler saves thread state and gracefully halts the whole runtime. The runtime will be re-activated once the `Promise` is fulfilled.

#### Add growable heap/garbage collection to storage manager

Currently, the heap size is fixed and can be specified by `ahc-link --heap-size`. By defaulting a heap size of 1GB, we pretend memory is infinite and focus work on other issues, but this can come back to bite us at any time.

There are two steps in this tasks:

* Implement growable heap. When GC is entered, we allocate fresh blocks and move the nursery/object pool to point to new blocks.
* Implement garbage collection. Porting all GC routines is a huge amount of work and error-prone, so we implement a non-generational one in JavaScript.

#### Solve reference leaking in JSFFI

`JSRef` is implemented much like `StablePtr`: a mapping from handles to objects. Whenever a `JSRef` enters Haskell land, the underlying object is pointed to by a mapping, but currently there's no mechanism to free a `JSRef`.

Some possible fixes:

* Provide `freeJSRef` in Haskell land, works for any individual `JSRef`
* Provide `nukeJSRefs`, can be called periodically to wipe all `JSRef`s
* Provide a region-based API, all `JSRef`s are tied to a region. Regions themselves can be allocated and recycled. Optionally there can be a global region.

#### Support 3rd-party GHC plugins

Currently there exist multiple plugin mechanisms in GHC:

* Frontend plugins, allowing one to create a custom major mode and execute custom login in the `Ghc` monad when the `ghc` process is called. GHC only does the work of parsing command line arguments.
* Core plugins, allowing one to modify the Core -> Core pipeline and insert custom passes.
* Source plugins, allowing one to inspect/modify the parsed/renamed/typechecked AST.
* Hooks, allowing one to override certain GHC internal functions.

We should add tests to ensure 3rd-party GHC plugins work.

* For Core plugins: we don't manipulate Core, so they should work out of the box.
* For Source plugins: the JSFFI mechanism relies on rewriting AST (currently parsed AST), but via `runPhaseHook` instead of source plugins, so this may work but needs some testing.
* For Frontend plugins: to support a frontend plugin, work must be done on the plugin side instead of here, since the main logic is handled by plugin itself. Not practical since we use hooks to implement our modified pipeline, which is likely to be hard to integrated into another pipeline.
* For Hooks: Not practical, reason is the same as above.

#### Implement Template Haskell/GHCi

There are two possible ways to implement Template Haskell:

* Link with the native code produced when booting. For simple `Q` computations that doesn't involve `runIO` this should work fine, but it won't work when one calls a WebAssembly computation in `Q`.
* Implement the remote interpreter for WebAssembly, much like ghcjs. When Template Haskell/GHCi is involved, we fire up a Node.js/Headless Chrome process and do all the message passing. This is the ideal solution but takes a huge amount of work.

Implementing the first approach is straightforward:

* While we compile to WebAssembly, we still perform native code generation and emit x64 object files. The emitted object files are just there to make `Cabal` happy, but GHCi linker can take advantage of them.
* GHCi linking for the objects should work out of the box (at least on Linux). There is a `th` test suite but it isn't run on CI mainly because of some weird Windows GHCi linker bug.
* As long as GHCi linker doesn't fail, the computation in `Q` monad can be executed in the `ahc` process. It's not run in a JavaScript runtime properly, so the splice's module must not have any transitive dependency on a JSFFI import (GHCi linking will fail anyway).

GHCi can work in a similar way (but pretty meaningless..)

#### Improve `Cabal` support

Currently, `Cabal` still thinks `ahc` is yet another `ghc` and feeds it with `ghc` command line arguments. We should teach it to regard `ahc` as a new Haskell compiler, and what to do for typical commands (`configure`/`build`/`install`, etc).

After `Cabal` support is improved, we can:

* Get away with current "boot libs" mechanism, instead rely on regular GHC package databases
* Give users ability to build/use packages outside boot libs
* Go on with improving `cabal-install`, some day a plain `cabal build --asterius` may work

#### Improve test suite

The current test suites have poor coverage of Haskell features.

### Archived tasks

The following tasks have lower priority, either due to low impact to end-user experience or significant time involved. They are archived here and may be revisited at a later date, and we're still happy to discuss or review a pull request.

#### Improve WebAssembly EDSL

We already have a monadic EDSL for constructing WebAssembly code. There are still minor flaws with current EDSL:

* No notion of `struct`s. We manually load/store via a base pointer and an offset.
* Not type-safe. It's possible to mix-up `I32`/`I64` stuff and it's not always possible for `binaryen` validator to catch the problem (especially when load/store is involved)
* Global/static variables need a lot of boilerplate

#### Switch away from `binaryen`

`binaryen` is a fantastic library for WebAssembly code generation and has powered `asterius` since the beginning. However, there are reasons to switch away and implement our own WebAssembly code generation library:

* The relooper has been a constant source of trouble. We already implement our own relooper now.
* There's no support for linking and symbol resolution, so we have to keep two sets of types for WebAssembly: one is our own for pre-linking modules, one is the final linked data to feed to `binaryen`, but they overlap a lot.
* `binaryen` is conservative in features. We'd like to try experimental WebAssembly features (exception handling, multi-return, anyref, etc) in V8.

#### Integrate LLVM/Clang or Emscripten

It's a shame we can't compile simple `cbits` in Haskell packages and have to hand-write WebAssembly code instead.

#### Implement "Try asterius" website

To increase momentum for this project, it'd be nice to have a "try asterius" website, where people can send snippets of modules and download compiled code to run in their browsers.

#### Add macOS support

Currently we don't build GHC bindists for macOS and test it on CircleCI. For the sake of macOS Haskellers this should be implemented.

#### Add Nix/Bazel support

It'd be nice to support building the project via Nix/Bazel.

#### Support `integer-gmp`

We currently use `integer-simple`, but not all packages implement the flags to switch away from `integer-gmp`.

It's worth mentioning that V8 already has experimental support for the BigInt proposal, so `Integer`s should ideally be powered by `bigint`s under the hood.

#### Support tables-next-to-code

Of course, we know the WebAssembly standard separates data and code, so something like tables-next-to-code won't work; but come to think of it, at link time we already know the absolute addresses of "code", so we can cheat a little bit here..

If we support both `integer-gmp` and tables-next-to-code, we can stop requiring users to set up a custom GHC first, and can distribute `asterius` as a vanilla package.
