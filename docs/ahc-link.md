# Using `ahc-dist`/`ahc-link`

`ahc-link` is the frontend program of Asterius. It taks a Haskell `Main` module and optionally an ES6 "entry" module as input, then emits a `.wasm` WebAssembly binary module and companion JavaScript, which can be run in Node.js or browser environments.

`ahc-dist` works similarly, except it takes the "executable" file generated from `ahc` (either directly by calling `ahc`, or indirectly by using `cabal`) as input. Most command-line arguments are the same as `ahc-link`, except `ahc-link` takes `--input-hs`, while `ahc-dist` takes `--input-exe`.

The options are described in full details here.

## Basic input/output options

### `--input-hs ARG`

The Haskell `Main` module's file path. This option doesn't have a default and is mandatory; all others are optional. This works only for `ahc-link`.

### `--input-exe ARG`

The "executable" file path. This works only for `ahc-dist`, and is also mandatory.

### `--input-mjs ARG`

The ES6 "entry" module's file path. If not specified, the default entry module initializes an Asterius instance and calls `Main.main`, and upon normal completion, prints the standard output via `console.log`.

It's possible to override the default behavior by specifying your own entry module. First, you need two imports (the relevant files are auto-generated and don't exist yet):

```
import { module } from "./xx.wasm.mjs";
import { newInstance } from "./xx.lib.mjs";
```

Assuming `xx.hs` is the input Haskell `Main` module.

Now, `module` is a `Promise` which resolves to a `WebAssembly.Module`. It's stateless and supports structured cloning, so it's possible to reuse it by send it to `Worker`s, store it in IndexedDB, etc. To avoid unnecessary compilation when reusing it, you need the dynamic `import()` for `xx.wasm.mjs` instead.

`newInstance` is an async function which takes the `WebAssembly.Module` and resolves to an Asterius instance. An Asterius instance is a super-set of a `WebAssembly.Instance` and contains stateful fields to support running Haskell code. You can take a look at the manually written entry modules in `asterius/test` to get some idea on the capabilities of such an instance.

### `--output-directory ARG`

Specifies the output directory. Defaults to the same directory of `--input-hs`.

### `--output-prefix` ARG

Specifies the prefix of the output files. Defaults to the base filename of `--input-hs`, so for `xx.hs`, we generate `xx.wasm`, `xx.lib.mjs`, etc.

## Common options for controlling outputs

### `--tail-calls`

Enable the WebAssembly tail call opcodes in output binary. This requires Node.js/Chromium to be built with a fairly recent revision of V8, and called with the `--experimental-wasm-return-call` flag. Doesn't work with the binaryen backend yet.

See the "Using experimental WebAssembly features" section for more details.

### `--ghc-option ARG`

Specify additional ghc options. The `{-# OPTIONS_GHC #-}` pragma also works.

### `--browser`

Indicates the output code is to be run in a browser environment. By default, the output code is intended to be run by Node.js instead.

### `--bundle`

Instead of copying the runtime `.mjs` modules to the target directory, generate a self-contained `xx.js` script, and running `xx.js` has the same effect as running the entry module. Only works for browser targets.

`--bundle` is supported by [`Parcel`](https://parceljs.org/) under the hood and performs minification on the bundled JavaScript file. It's likely beneficial since it reduces total size of scripts and doesn't require multiple requests for fetching them.

## More advanced options for hackers

### `--run`

Runs the output code using `node`. Ignored for browser targets.

### `--binaryen`

Use the binaryen backend for generating `.wasm` files. Also note that with the binaryen backend, we use the binaryen relooper instead of our own relooper, which at the moment may give better runtime performance.

If you observe any different runtime behavior of output code when this option is on, it's a bug!

### `--debug`

Switch on the debug mode. Emits a ton of event logs suitable for piping to `grep` (or just leave it on the screen in case you'd like some hypnosis)

### `--full-sym-table`

Contain the full symbol table into `xx.lib.mjs`. Automatically implied by `--debug`.

### `--no-gc-sections`

Do not run dead code elimination.

## Options affecting the linker

### `--export-function ARG`

Use this when you `foreign export javascript` anything. Otherwise the dead code elimination performed by the linker will surely exclude that code from the output WebAssembly module if it's not transitively used by `Main.main`!

### `--extra-root-symbol ARG`

Use this to specify a symbol to be added to the "root symbol set". Works similar to `--export-function`, but the argument is not a Haskell function name, but a symbol name directly.

## Additional outputs for the curious

### `--output-link-report`

Output a "link report" text file containing internal linker stats.

### `--output-ir`

Output wasm IRs of compiled Haskell modules and the resulting module. The IRs aren't intended to be consumed by external tools like binaryen/wabt.
