# JavaScript FFI

Asterius implements JSFFI, which enables importing sync/async JavaScript code,
and exporting static/dynamic Haskell functions. The JSFFI syntax and semantics
is inspired by JSFFI in GHCJS, but there differ in certain ways.

## Marshaling data between Haskell and JavaScript

### Directly marshalable value types

There are mainly 3 kinds of marshalable value types which can be directly used
as function arguments and return values in either JSFFI imports or exports:

* Regular Haskell value types like `Int`, `Ptr`, `StablePtr`, etc. When the
  `MagicHash` and `UnliftedFFITypes` extensions are enabled, some unboxed types
  like `Int#` is also supported.
* The `JSVal` type and its `newtype`s.
* The `Any` type.

The `JSVal` type is exported by
[`Asterius.Types`](https://github.com/tweag/asterius/blob/master/ghc-toolkit/boot-libs/base/Asterius/Types.hs).
It represents an opaque JavaScript value in the Haskell world; one can use JSFFI
imports to obtain `JSVal` values, pass them across Haskell/JavaScript, store
them in Haskell data structures like ordinary Haskell values. `JSVal`s are
garbage collected, but it's also possible to call `freeJSVal` to explicitly free
them in the runtime.

The `Any` type in `GHC.Exts` represents a boxed Haskell value, which is a
managed pointer into the heap. This is only intended to be used by power users.

Just like regular `ccall` imports/exports, the result type of `javascript`
imports/exports can be wrapped in `IO` or not.

### The `JSVal` family of types

Other than `JSVal`, `Asterius.Types` additionally exports these types:

* `JSArray`
* `JSFunction`
* `JSObject`
* `JSString`
* `JSUint8Array`

They are `newtype`s of `JSVal` and can be directly used as argument or result
types as well. The runtime doesn't perform type-checking at the JavaScript side,
e.g. it won't check if `typeof $1 === "string"` when `$1` is declared as a
`JSString`. It's up to the users to guarantee the runtime invariants about such
`JSVal` wrapper types.

User-defined `newtype`s of `JSVal` can also be used as marshalable value types,
as long as the `newtype` constructor is available in scope.

### Marshaling structured data

Given the ability of passing simple value types, one can implement their own
utilities for passing a piece of structured data either from JavaScript to
Haskell, or vice versa.

To build a Haskell data structure from a JavaScript value, usually we write a
builder function which recursively traverses the substructure of the JavaScript
value (sequence, tree, etc) and build up the Haskell structure, passing one cell
at a time. Similarly, to pass a Haskell data structure to JavaScript, we
traverse the Haskell data structure and build up the JavaScript value.

The Asterius standard library provides functions for common marshaling purposes:

```haskell
import Asterius.Aeson
import Asterius.ByteString
import Asterius.Text
import Asterius.Types

fromJSArray :: JSArray -> [JSVal]
toJSArray :: [JSVal] -> JSArray
fromJSString :: JSString -> String
toJSString :: String -> JSString
byteStringFromJSUint8Array :: JSUint8Array -> IO ByteString
byteStringToJSUint8Array :: ByteString -> IO JSUint8Array
textFromJSString :: JSString -> Text
textToJSString :: Text -> JSString
jsonToJSVal :: ToJSON a => a -> JSVal
jsonFromJSVal :: FromJSON a => JSVal -> Either String a
jsonFromJSVal' :: FromJSON a => JSVal -> a
```

### The 64-bit integer precision problem

Keep in mind that when passing 64-bit integers via `Int`, `Word`, etc, precision
can be lost, since they're represented by `number`s on the JavaScript side. In
the future, we may consider using `bigint`s instead of `number`s as the
JavaScript representations of 64-bit integers to solve this issue.

## JSFFI imports

### JSFFI import syntax

```haskell
import Asterius.Types

foreign import javascript unsafe "new Date()" current_time :: IO JSVal

foreign import javascript interruptible "fetch($1)" fetch :: JSString -> IO JSVal
```

The source text of `foreign import javascript` should be a single valid
JavaScript expression, using `$n` to refer to the `n`-th argument (starting from
`1`). It's possible to use IIFE(Immediately Invoked Function Expression) in the
source text, so more advanced JavaScript constructs can be used.

### Sync/async JSFFI imports

The safety level in a `foreign import javascript` declaration indicates whether
the JavaScript logic is asynchronous. When omitted, the default is `unsafe`,
which means the JavaScript code will return the result synchronously. When
calling an `unsafe` import, the whole runtime blocks until the result is
returned from JavaScript.

The `safe` and `interruptible` levels mean the JavaScript code should return a
`Promise` which later resolves with the result. The current thread will be
suspended when such an import function is called, and resumed when the `Promise`
resolves or rejects. Other threads may continue execution when a thread is
blocked by a call to an async import.

### Error handling in JSFFI imports

When calling a JSFFI import function, The JavaScript code may synchronously
throw exceptions or reject the `Promise` with errors. They are wrapped as
`JSException`s and thrown in the calling thread, and the `JSException`s can be
handled like regular synchronous exceptions in Haskell. `JSException` is also
exported by `Asterius.Types`; it contains both a `JSVal` reference to the
original JavaScript exception/rejection value, and a `String` representation of
the error, possibly including a JavaScript stack trace.

### Accessing the asterius instance object

In the source text of a `foreign import javascript` declaration, one can access
everything in the global scope and the function arguments. Additionally, there
is an `__asterius_jsffi` binding which represents the asterius instance object.
`__asterius_jsffi` exposes certain interfaces for power users, e.g.
`__asterius_jsffi.exposeMemory()` which exposes a memory region as a JavaScript
typed array. The interfaces are largely undocumented and not likely to be useful
to regular users.

There is one usage of `__asterius_jsffi` which may be useful to regular users
though. Say that we'd like the JSFFI import code to call some 3rd-party library
code, but we don't want to pollute the global scope; we can assign the library
functions as additional fields of the asterius instance object after it's
returned by `newAsteriusInstance()`, then access them using `__asterius_jsffi`
in the JSFFI import code.

## JSFFI exports

### JSFFI static exports

```haskell
foreign export javascript "mult_hs" (*) :: Int -> Int -> Int
```

The `foreign export javascript` syntax can be used for exporting a static top-level Haskell function to JavaScript. The source text is the export function name, which must be globally unique. The supported export function types are the same with JSFFI imports.

For the exported functions we need to call in JavaScript, at link-time, each
exported function needs an additional `--export-function` flag to be passed to
`ahc-link`/`ahc-dist`, e.g. `--export-function=mult_hs`.

In JavaScript, after `newAsteriusInstance()` returns the asterius instance
object, one can access the exported functions in the `exports` field:

```javascript
i.exports.hs_init();
const r = await i.exports.mult_hs(6, 7);
```

Note that all exported Haskell functions are async JavaScript functions. The
returned `Promise` resolves with the result when the thread successfully
returns; otherwise it may reject with a JavaScript string, which is the
serialized form of the Haskell exception if present.

It's safe to call a JSFFI export function multiple times, or call another JSFFI
export function before a previous call resolves/rejects. The export functions
can be passed around as first-class JavaScript values, called as ordinary
JavaScript functions or indirectly as JavaScript callbacks. They can even be
imported back to Haskell as `JSVal`s and called in Haskell.

### JSFFI dynamic exports

```haskell
import Asterius.Types

foreign import javascript "wrapper" makeCallback :: (JSVal -> IO ()) -> IO JSFunction
foreign import javascript "wrapper oneshot" makeOneshotCallback :: (JSVal -> IO ()) -> IO JSFunction

freeHaskellCallback :: JSFunction -> IO ()
```

The `foreign import javascript "wrapper"` syntax can be used for exporting a
Haskell function closure to a JavaScript function dynamically. The type
signature must be of the form `Fun -> IO JSVal`, where `Fun` represents a
marshalable JSFFI function type in either JSFFI imports or static exports, and
the result can be `JSVal` or its `newtype`.

After declaring the "wrapper" function, one can pass a Haskell function closure
to it and obtain the `JSVal` reference of the exported JavaScript function. The
exported function can be used in the same way as the JSFFI static exports.

When a JSFFI dynamic export is no longer useful, call `freeHaskellCallback` to
free it. The `JSVal` reference of the JavaScript callback as well as the
`StablePtr` of the Haskell closure will be freed.

Sometimes, we expect a JSFFI dynamic export to be one-shot, being called for
only once. For such one-shot exports, use `foreign import javascript "wrapper
oneshot"`. The runtime will automatically free the resources once the exported
JavaScript is invoked, and there'll be no need to manually call
`freeHaskellCallback` for one-shot exports.

## Implementation

TODO: legacy content ahead!

This subsection presents a high-level overview on the implementation of JSFFI,
based on the information flow from syntactic sugar to generated
WebAssembly/JavaScript code. It's not a required reading for *users* of the
JSFFI feature.

### Hooking typechecker/desugarer

Vanilla GHC doesn't support typechecking/desugaring foreign declarations whose
calling convention is `javascript`. However, it does provide `dsForeignsHook`,
`tcForeignImportsHook` and `tcForeignExportsHook` for overriding
typechecker/desugarer behavior. The mechanism was intended for use by ghcjs.
We've implemented our JSFFI-related hooks in
[`Asterius.Foreign`](https://github.com/tweag/asterius/blob/master/asterius/src/Asterius/Foreign.hs).

### Rewriting from JSFFI to C FFI

As documented in previous sections, one can write `foreign import javascript` or
`foreign export javascript` clauses in a `.hs` module. The logic to process this
syntax at compile-time resides in
[`Asterius.Foreign.Internals`](https://github.com/tweag/asterius/blob/master/asterius/src/Asterius/Foreign/Internals.hs).

The hooks we described in the previous subsection imports
`Asterius.Foreign.Internals`, and call `processFFIImport` or `processFFIExport`
when it's handling a foreign declaration. When the calling convention is
`javascript`, they parse the type signature, store the info in a mutable
variable, and then rewrite the original declaration's calling convention to
normal `ccall`, so subsequent desugar and codegen can proceed as normal.

After all foreign declarations in a module are processed, the relevant
`FFIMarshalState` indexed by the module name can be fetched, and a "stub module"
of type `AsteriusModule` can be fetched given the current
`AsteriusModuleSymbol`. Both `AsteriusModule` and `FFIMarshalState` types has
`Semigroup` & `Monoid` instances so they can be combined later at link-time.

### The JSFFI types

In
[`Asterius.Types`](https://github.com/tweag/asterius/blob/master/asterius/src/Asterius/Types.hs),
we define `FFIValueType` and `FFIFunctionType` to represent the JSFFI argument &
return value types, and the import/export function types. They contain more
information than normal `ValueType` and `FunctionType` which we use for emitting
normal wasm code:

* `FFIValueType` has two variants: `FFI_VAL` and `FFI_JSVAL`. `FFI_VAL`
  represents a "value type" like `Int`, `Bool`, etc, while `FFI_JSVAL`
  represents `JSVal` and its `newtype`s. `FFI_JSVAL` requires special handling
  since it represents an opaque JavaScript value on the Haskell heap.
* `FFIFunctionType` additionally contains a flag indicating whether the returned
  value is wrapped in `IO` or not. This matters for exported functions; although
  `IO` or not doesn't affect the way of calling the exported functions in
  JavaScript, internally, different `rts_eval*` functions are needed for
  triggering evaluation.

The `parseFFIValueType` function is called to marshal a pair of Haskell types to
an `FFIValueType`. When parsing JSFFI types from Haskell types, we mainly check
the normalized Haskell type which strips away type synonyms and newtypes; but we
also need to check the pre-normalization type, since `JSVal` is still a newtype
of `StablePtr`, but it can't be recognized in the normalized type.

### Across the JavaScript/WebAssembly boundary

Within the WebAssembly MVP standard, only `i32`/`f32`/`f64` is permitted to be
passed across the JavaScript/WebAssembly boundary as arguments & return values.
`i64` doesn't work by default, but unfortunately, asterius works with 64-bit
words, so `i64` is the most common runtime value type.

To workaround the `i64` restriction in JSFFI imports and exports, we convert it
to `f64` when passing it between JavaScript and WebAssembly. The conversion
takes into account whether the Haskell type is signed or not (e.g. for `Word` we
can use `ConvertUInt64ToFloat64`/`TruncUFloat64ToInt64` opcodes for conversion).
For now, there's a risk of overflowing `Number.MAX_SAFE_INTEGER`.

`JSVal` values get similar treatment; they are treated as 64-bit words on the
Haskell heap, but in the JavaScript runtime, we maintain a `Map` from the
indices to real JavaScript values. When calling a JSFFI import in Haskell or a
JSFFI export in JavaScript, the conversion between indices and real JavaScript
values is handled automatically.

### Generating JavaScript/WebAssembly code related to imports & exports

For JSFFI imports, we generate an "import object factory" JavaScript function in
`generateFFIImportObjectFactory`. That function takes the `__asterius_jsffi`
object as argument, and outputs a WebAssembly import object. The JSFFI related
import functions reside in the `jsffi` "module". At runtime, the factory
function is called with the asterius instance object, and the returned import
object will be used to instantiate the `WebAssembly.Instance`.

For JSFFI exports, each export is mapped to an async JavaScript function to be
made available as a member function of the instance's `.exports` property.
