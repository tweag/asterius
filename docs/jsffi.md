# JavaScript FFI

There is a prototype implementation of `foreign import javascript` right now, check the `jsffi`/`teletype` unit tests for details. The syntax is like:

```
import Asterius.Types

foreign import javascript "new Date()" current_time :: IO JSVal

foreign import javascript "console.log(${1})" js_print :: JSVal -> IO ()
```

The source text of `foreign import javascript` should be a valid JavaScript expression. You can use `${n}` to refer to the nth function parameter starting from 1. By using the IIFE(Immediately Invoked Function Expression) design pattern, it's even possible to define local variables and write `for` loops.

Supported basic types are:

* `Ptr`
* `FunPtr`
* `StablePtr`
* `Bool`
* `Int`
* `Word`
* `Char`
* `Float`
* `Double`
* `JSVal`/`JSArrayBuffer`/`JSString`/`JSArray`

For the lifted basic types, the result can be wrapped in `IO` (or not). There's also limited support for unlifted FFI types:

* `StablePtr# a`
* `Addr#`
* `ByteArray#`
* `MutableByteArray# s`
* `Char#`
* `Int#`
* `Word#`
* `Float#`
* `Double#`

These unlifted FFI types can be used in a `foreign import javascript` clause (but not `export`.) The results can't be wrapped in `IO`.

`JSVal` is defined in `Asterius.Types` in the patched `ghc-prim` package. In the Haskell land, `JSVal` is first-class and opaque: you can pass it around, put it in a data structure, etc, but under the hood it's just a handle. The runtime maintains mappings from handles to real JavaScript objects.

Normally, the Haskell FFI mechanism permits defining `newtype`s to the marshallable basic types, and the wrapping/unwrapping is done automatically. However, this doesn't work yet due to the way we implement JSFFI right now. You *can* define a `newtype` to `JSVal`/`JSWhatever`, but in a `foreign import javascript`/`foreign export javascript` declaration, you still must use one of the builtin `JS*` types, and when using imported functions, you need to manually `coerce` them (`coerce` works when `Asterius.Types` is imported).

Also, a prototype of `foreign export javascript` is implemented, check `jsffi` for details. The syntax is roughly:

```
foreign export javascript "mult_hs" (*) :: Int -> Int -> Int
```

In a Haskell module, one can specify the exported function name (must be globally unique), along with its Haskell identifier and type. One can specify `ahc-link --export-function=mult_hs` to make the linker include the relevant bits in final WebAssembly binary, and export `mult_hs` as a regular WebAssembly export function. After calling `hs_init` to initialize the runtime, one can call `mult_hs` just like a regular JavaScript function.

## Converting between Haskell and JavaScript types

The `Asterius.Types`/`Asterius.ByteString` modules provide some high-level functions for converting between Haskell and JavaScript types:

```
fromJSString :: JSString -> [Char]
toJSString :: [Char] -> JSString

fromJSArray :: JSArray -> [JSVal]
toJSArray :: [JSVal] -> JSArray

byteStringFromJSArrayBuffer :: JSArrayBuffer -> ByteString
byteStringToJSArrayBuffer :: ByteString -> JSArrayBuffer
```

It's possible to define them just by using the basic JSFFI mechanism, but those functions are backed by special runtime interfaces which makes them a lot faster. Most notably, the `fromJS*` functions directly traverse the JavaScript value and build a fully-evaluated Haskell data structure on the heap in one pass.

## What's permitted in `foreign import javascript`

In a `foreign import javascript` declaration, you can access all properties of the global object (`window` in browsers, `global` in node.js), so all functionalities of standard JavaScript is permitted.

Additionally, the `__asterius_jsffi` object is in scope; it is initialized before instantiating the WebAssembly instance, and contains the runtime interfaces used to support the JSFFI features (e.g. manipulation of `JSVal`s). You may check `rts/rts.js` to see what `__asterius_jsffi` contains, but we don't recommend using it in your code since it's intended to be an implementation detail; shall you feel the need to access it, please file an issue instead and we'll add your missing functionality as proper Haskell/JavaScript interfaces instead.

## Implementation

This subsection presents a high-level overview on the implementation of JSFFI, based on the information flow from syntactic sugar to generated WebAssembly/JavaScript code. It's not a required reading for *users* of the JSFFI feature.

### Syntactic sugar

As documented in previous sections, one can write `foreign import javascript` or `foreign export javascript` clauses in a `.hs` module. How are they processed? The logic resides in `Asterius.JSFFI`.

First, there is `addFFIProcessor`, which given a `Compiler` (defined in `ghc-toolkit`), returns a new `Compiler` and a callback to fetch a stub module. The details of `Compiler`'s implementation are not relevant here, just think of it as an abstraction layer to fetch/modify GHC IRs without dealing with all the details of GHC API.

`addFFIProcessor` adds one functionality to the input `Compiler`: rewrite parsed Haskell AST and handle the `foreign import javascript`/`foreign export javascript` syntactic sugar. After rewriting, JavaScript FFI is really turned into C FFI, so type-checking/code generation proceeds as normal.

After the parsed AST is processed, a "stub module" of type `AsteriusModule` is generated and can be later fetched given an `AsteriusModuleSymbol`. It contains JSFFI related information of type `FFIMarshalState`. Both `AsteriusModule` and `FFIMarshalState` types has `Semigroup` instance so they can be combined later at link-time.

### TODO

### Adding a JSFFI basic type

Look at the following places:

* `Asterius.JSFFI` module. All JavaScript reference types are uniformly handled as `FFI_JSREF`, while value types are treated as `FFI_VAL`. Assuming we are adding a value type. Add logic to:
    * `marshalToFFIValueType`: Recognize the value type in parsed AST, and translate to `FFI_VAL`
* `Asterius.Builtins` module. Add the corresponding `rts_mkXX`/`rts_getXX` builtin functions. They are required for stub functions of `foreign export javascript`.
