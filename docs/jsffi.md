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
  like `Int#` are also supported.
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
byteStringFromJSUint8Array :: JSUint8Array -> ByteString
byteStringToJSUint8Array :: ByteString -> JSUint8Array
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
is an `__asterius_jsffi` binding which represents the Asterius instance object.
`__asterius_jsffi` exposes certain interfaces for power users, e.g.
`__asterius_jsffi.exposeMemory()` which exposes a memory region as a JavaScript
typed array. The interfaces are largely undocumented and not likely to be useful
to regular users.

There is one usage of `__asterius_jsffi` which may be useful to regular users
though. Say that we'd like the JSFFI import code to call some 3rd-party library
code, but we don't want to pollute the global scope; we can assign the library
functions as additional fields of the Asterius instance object after it's
returned by `newAsteriusInstance()`, then access them using `__asterius_jsffi`
in the JSFFI import code.

## JSFFI exports

### JSFFI static exports

```haskell
foreign export javascript "mult_hs" (*) :: Int -> Int -> Int
```

The `foreign export javascript` syntax can be used for exporting a static
top-level Haskell function to JavaScript. The source text is the export
function name, which must be globally unique. The supported export function
types are the same with JSFFI imports.

For the exported functions we need to call in JavaScript, at link-time, each
exported function needs an additional `--export-function` flag to be passed to
`ahc-link`/`ahc-dist`, e.g. `--export-function=mult_hs`.

In JavaScript, after `newAsteriusInstance()` returns the Asterius instance
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
