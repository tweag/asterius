# JavaScript FFI

## JSFFI Types

These types can be used as arguments or return values in either JSFFI imports or
exports:

* `Ptr`
* `FunPtr`
* `StablePtr`
* `Bool`
* `Int`
* `Word`
* `Char`
* `Float`
* `Double`

In addition, these types in
[`Asterius.Types`](https://github.com/tweag/asterius/blob/master/ghc-toolkit/boot-libs/base/Asterius/Types.hs)
are supported; they represent opaque JavaScript values:

* `JSVal`
* `JSArrayBuffer`
* `JSString`
* `JSArray`
* `JSObject`
* `JSFunction`

All `JS*` types are newtypes of `JSVal`, and there's currently no runtime
typechecking to enforce e.g. an actual array is passed via `JSArray`.

The result can be wrapped in `IO` or not. The usual caveats of C FFI regarding
`IO` also apply here.

It's worth noting that when writing JSFFI imports/exports, no type synonyms or
newtypes can be used for JSFFI types yet. The restriction rises from the hacky
way we implement JSFFI: processing the `HsSyn` AST at the parser phase, rather
than renamer/typechecker, therefore we only recognize types by strings at the
moment. This restriction will be solved in the long run; meanwhile, the common
practice of making newtype wrappers for distinction of various JavaScript types
is still possible via separating the actual interface with the underlying
import, and using `coerce` or manual wrapping/unwrapping.

When the `MagicHash`/`UnliftedFFITypes` extensions are on, these unlifted types
can also be used as JSFFI types. However, when used as return types, the result
can't be wrapped in `IO`.

* `StablePtr# a`
* `Addr#`
* `ByteArray#`
* `MutableByteArray# s`
* `Char#`
* `Int#`
* `Word#`
* `Float#`
* `Double#`

## JSFFI Imports

```haskell
import Asterius.Types

foreign import javascript "new Date()" current_time :: IO JSVal

foreign import javascript interruptible "fetch(${1})" fetch :: JSVal -> IO JSVal
```

The source text of `foreign import javascript` should be a single valid
JavaScript expression, using `${n}` to refer to the `n`-th argument (starting
from `1`). It's possible to use IIFE(Immediately Invoked Function Expression) to
define local variables and use advanced control-flow features here, but complex
logic should really appear in standalone scripts.

In the source text, all properties of the global object can be accessed, and
additionally, the `__asterius_jsffi` identifier can be used to refer to the
current "asterius instance". Say that we want to bind a JavaScript library `X`
to Haskell without polluting the global namespace, we can simply initialize the
asterius instance, assign `X` as a property of the instance object, then in the
source text, use `__asterius_jsffi.X` to refer to it.

Unlike C FFI which defaults to the `safe` safety level, when the safety level is
omitted in a `foreign import javascript` declaration, the default level is
`unsafe`. Asterius currently treats `unsafe` imports as synchronous imports, and
`safe`/`interruptible` imports as asynchronous imports.

The synchronous imports are lightweight; each call to a synchronous import maps
to a call to a WebAssembly import corresponding to the function generated using
the source text. The JavaScript code is assumed to return a value of the correct
type synchronously; failing to do so, by either returning ill-typed values or
throwing errors, will result in unrecoverable runtime errors.

The asynchronous imports are based on JavaScript `Promise`s. The JavaScript code
is assumed to return a `Promise` which resolves to the correct result type; it's
wrapped in a `Promise.resolve` call in the generated function, so normal
synchronous code works too. Upon an asynchronous call, the Haskell execution is
suspended, and resumed when the `Promise` is resolved or rejected. If the
`Promise` is rejected, the error is wrapped in a `JSException` (defined in
`Asterius.Types`) and thrown in the Haskell calling thread.

## JSFFI Exports

```haskell
foreign export javascript "mult_hs" (*) :: Int -> Int -> Int
```

In a Haskell module, one can specify the exported function name (must be
globally unique), along with its Haskell identifier and type. One must specify
`ahc-link --export-function=mult_hs` to make the linker include the relevant
bits in final WebAssembly binary, and export `mult_hs` as a regular WebAssembly
export function. After calling `hs_init` to initialize the runtime, one can call
`mult_hs` just like a regular JavaScript async function:

```javascript
i.exports.hs_init();
const r = await i.exports.mult_hs(6, 7);
```

It's also possible to use `JS*` types in JSFFI exports. In that case, when
calling the exported function in `i.exports`, we can directly pass the
JavaScript values as arguments, and receive them as results, without having to
care about the bijection between `JSVal` ids and actual values.

## Marshaling between Haskell and JavaScript types

The `Asterius.Types`/`Asterius.ByteString` modules provide some high-level
functions for converting between Haskell and JavaScript types:

```haskell
fromJSString :: JSString -> [Char]
toJSString :: [Char] -> JSString

fromJSArray :: JSArray -> [JSVal]
toJSArray :: [JSVal] -> JSArray

byteStringFromJSArrayBuffer :: JSArrayBuffer -> ByteString
byteStringToJSArrayBuffer :: ByteString -> JSArrayBuffer
```

It's possible to define them just by using the basic JSFFI mechanism, but those
functions are backed by special runtime interfaces which makes them a lot
faster. Most notably, the `fromJS*` functions directly traverse the JavaScript
value and build a fully-evaluated Haskell data structure on the heap in one
pass.

## Implementation

This subsection presents a high-level overview on the implementation of JSFFI,
based on the information flow from syntactic sugar to generated
WebAssembly/JavaScript code. It's not a required reading for *users* of the
JSFFI feature.

### Rewriting from JSFFI to C FFI

As documented in previous sections, one can write `foreign import javascript` or
`foreign export javascript` clauses in a `.hs` module. The logic to process this
syntax resides in
[`Asterius.JSFFI`](https://github.com/tweag/asterius/blob/master/asterius/src/Asterius/JSFFI.hs).

First, the entry to JSFFI-related logic is `addFFIProcessor`, which given a
`Compiler` (defined in `ghc-toolkit`), returns a new `Compiler` and a callback
to fetch a stub module. The details of `Compiler`'s implementation are not
relevant here, just think of it as some sort of "middleware" to fetch/modify GHC
IRs without dealing with all the details of GHC API.

`addFFIProcessor` layers a specific functionality upon the input `Compiler`:
rewrite *parsed* Haskell AST and handle the `foreign import javascript` &
`foreign export javascript` syntactic sugar. After rewriting, JavaScript FFI is
really turned into C FFI under the hood, so type-checking and desugaring
proceeds as normal.

After the parsed AST is processed, a "stub module" of type `AsteriusModule` is
generated and can be later fetched given the current `AsteriusModuleSymbol`. The
stub module contains JSFFI related information of type `FFIMarshalState`. Both
`AsteriusModule` and `FFIMarshalState` types has `Semigroup` & `Monoid`
instances so they can be combined later at link-time.

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

The `marshalToFFIValueType` function is called to marshal a Haskell type in the
parsed JSFFI declaration to an `FFIValueType`. It recognizes the Haskell types
by strings, e.g. if it is `Int` then we consider it an `Int`, if it starts with
`JS` then we consider it a `JSVal`. Since we're working with only a parsed AST
here and don't have access to the renamer/type-checker environments, it's
impossible to properly deal with type synonyms and `newtype`s, so if you define
`type MyInt = Int` and use `MyInt` in a JSFFI declaration, it will choke the
compiler.

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
