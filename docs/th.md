## Template Haskell implementation in asterius

### `ahc`

`ahc` is our `ghc` wrapper, and can be called directly or via Cabal to produce
library archives and executables.

#### Loading the frontend plugin

The entry point of `ahc` is `Language.Haskell.GHC.Toolkit.FakeGHC.fakeGHC`. It
takes a `FrontendPlugin` and generates the `main` function of a `ghc` wrapper.
When the wrapper is called with `--make`, it does its own command-line argument
parsing, then the frontend plugin does the rest. Otherwise it just calls the
real `ghc` with the same arguments.

`ghc` supports loading frontend plugins as a major mode, but we don't use that
since it used to cause cryptic boot failures, when `ghc` confuses the frontend
plugin's pkgdb with the wasm pkgdb.

#### The frontend plugin

`Asterius.FrontendPlugin` contains the main `ahc` logic. We use a few hooks
here, and the ones relevant to wasm compilation are:

* `cmmToRawCmmHook`: We obtain raw Cmm as the starting point of wasm codegen.
  This hook is only used for regular Haskell/Cmm sources, not splices.
* `runPhaseHook`: We modify the pipeline for compiling a single source file.
* `hscCompileCoreExprHook`: We modify the logic of "compiling the `CoreExpr` of
  a single TH splice and returning its foreign ref".

For TH splices, each splice's desugar output is sent to
`hscCompileCoreExprHook`.

#### Compiling `CoreExpr` of splices

`Asterius.Iserv.CompileCoreExpr.compileCoreExpr` contains the logic of compiling
splices to wasm. It generates a unique module/binder name, goes through the
pipeline of Core/STG/Cmm/Wasm. In the end, we obtain a `AsteriusModule` which is
the in-memory wasm object of the splice, and the object surely references back
into the `template-haskell` package.

When we have the wasm object, we send it to `ahc-iserv` via a `CreateBCOs`
message (it's not really BCO though). `ahc-iserv` stores the object and returns
an `HValueRef` which later can be used in a `RunTH` message.

### `ahc-iserv`

`ahc-iserv` is the `iserv` process of asterius. It's managed by an `ahc`
process.

#### Handling TH messages

`Asterius.Iserv.Run.run` is the main entry of `ahc-iserv`, implementing handlers
for a subset of TH messages.

### `node`
