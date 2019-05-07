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

`run` also takes a piece of `IservState` which is initialized when the
`ahc-iserv` process is started.

#### `IservState`

`IservState` is defined in `Asterius.Iserv.State` and contains two parts: one is
the session state we communicate with `node`, the other is the modules loaded so
far.

The session state is `JSSession` in the `inline-js-core` package. It manages a
`node` process, and we can send JavaScript code to `node`, evaluate it, exchange
binary data, etc.

The loaded modules come from 3 different TH messages:
`LoadLibrary`/`LoadObj`/`CreateBCOs`. When receiving a `LoadLibrary`/`LoadObj`
message, we read the archive/object file, deserialize it and store it in the
state, no linking is performed yet. The objects are indexed by filepaths and
thus can be removed upon `UnloadObj`, but archives are combined since there
isn't `UnloadLibrary`.

`CreateBCOs` contains the serialized `AsteriusModule` for splices. It's also
stored in the session state for later linking/executing.

#### Linking for TH

When handling a `RunTH` message, the first thing we do is linking: starting from
the splice's closure symbol indexed by the `HValueRef` returned earlier, we
obtain a self-contained `AsteriusModule`. This linking process reuses the
linking logic of regular Haskell main modules.

Even when we have an `AsteriusModule`, we can't run it yet; the wasm files are
always acompanied by some js modules, some copied and some generated. So we also
generate the js modules.

The logic of TH linking is in `Asterius.JSRun.NonMain`. It implements the logic
of linking for "non-main" entries: we aren't linking for main modules and don't
have `Main_main_closure`, but we can specify other symbols (e.g. TH splice
closure, TH runner closure, etc), and those symbols along with their deps will
end up in the linker output.

#### TH runner

Besides the closures of splices, we also need TH runners which can run something
like `Q Exp` and return the result in `IO`. Currently we have minimalistic TH
runners in the `Asterius.GHCi` module in `ghci` boot lib. They are simple
functions with a type signature like `Q Exp -> IO JSArrayBuffer`.

To use these TH runners, we also set the runner symbol (e.g.
`ghci_AsteriusziGHCi_asteriusRunQExp_closure`) as a linker root symbol, then we
use `rts_apply` to create a thunk which applies the runner to the splice. The
thunk can then be forced to WHNF with `rts_evalIO`. The resulting
`JSArrayBuffer` comes from the `ByteString` which is the result serialized via
its `binary` instance. Then we can obtain the real underlying `ArrayBuffer` in
JavaScript, and return it to the `iserv` process using `inline-js-core`.

These runner functions always initiate a dummy TH state when running a splice.
And all `Quasi` class methods (which calls `ghcCmd` to send queries back to the
host `ghc` process) aren't implemented yet. Thus only trivial splices which
don't call back into the host `ghc` are expected to work at the moment.

#### Running splices

Now that all components related to TH are introduced, how the splices are run
shall be obvious.

Upon a `RunTH` message, `ahc-iserv` combines all loaded libraries/objects along
with the splice and perform linking, producing a wasm binary file and some js
files in a temporary directory. It then loads the wasm/js files into a `node`
process, creates an asterius instance, call `hs_init()`, then create the closure
and evaluate it, finally returning the serialized result.

The running logic is `Asterius.Iserv.Run` and `Asterius.JSRun.NonMain`. An extra
thing worth noting: we also have a `Asterius.JSRun.Main` which contains similar
runners capable of loading wasm/js into `node` using `inline-js-core` and
retrieving "standard output" back to Haskell. The "main" runners are proven to
work.
