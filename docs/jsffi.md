## JavaScript FFI

There is a prototype implementation of `foreign import javascript` right now, check the `jsffi` test suite for details. The syntax is like:

```
foreign import javascript "new Date()" current_time :: IO JSRef

foreign import javascript "console.log(${1})" js_print :: JSRef -> IO ()
```

The source text of `foreign import javascript` should be a valid JavaScript expression (but you can use something like `${1}`, `${2}` to refer to the function parameters). Supported basic types are:

* `Ptr`
* `FunPtr`
* `StablePtr`
* `Bool`
* `Int`
* `Word`
* `Char`
* `Float`
* `Double`
* `JSRef`

The result can be wrapped in `IO` (or not).

`JSRef` is a magic type that doesn't actually appear in any module's source code. In the Haskell land, `JSRef` is first-class and opaque: you can pass it around, put it in a data structure, etc, but under the hood it's just a handle. The runtime maintains mappings from handles to real JavaScript objects.

Note that it's currently impossible to properly define `newtype`s to `JSRef` and use them in JSFFI import/export declarations. The asterius compiler rewrites all `JSRef` to `Int` at parse time, which doesn't play well with the ordinary FFI type checking mechanism. We need to stay with type synonyms at the moment.

Also, a prototype of `foreign export javascript` is implemented, check `jsffi` for details. The syntax is roughly:

```
foreign export javascript "mult_hs" (*) :: Int -> Int -> Int
```

In a Haskell module, one can specify the exported function name (must be globally unique), along with its Haskell identifier and type. One can specify `ahc-link --export-function=mult_hs` to make the linker include the relevant bits in final WebAssembly binary, and export `mult_hs` as a regular WebAssembly export function. After calling `hs_init` to initialize the runtime, one can call `mult_hs` just like a regular JavaScript function.

### Adding a JSFFI basic type

Look at the following places:

* `Asterius.JSFFI` module. All JavaScript reference types are uniformly handled as `FFI_JSREF`, while value types are treated as `FFI_VAL`. Assuming we are adding a value type. Add logic to:
    * `marshalToFFIValueType`: Recognize the value type in parsed AST, and translate to `FFI_VAL`
* `Asterius.Builtins` module. Add the corresponding `rts_mkXX`/`rts_getXX` builtin functions.
