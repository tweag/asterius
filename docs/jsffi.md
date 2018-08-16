## JavaScript FFI

There is a prototype implementation of `foreign import javascript` right now, check the `jsffi` test suite for details. The syntax is like:

```
foreign import javascript "new Date()" current_time :: IO JSRef

foreign import javascript "console.log(${1})" js_print :: JSRef -> IO ()
```

The source text of `foreign import javascript` should be a valid JavaScript expression (but you can use something like `${1}`, `${2}` to refer to the function parameters). Supported types are:

* `Ptr`
* `FunPtr`
* `StablePtr`
* `Int`
* `Word`
* `Char`
* `Float`
* `Double`
* `JSRef`

`JSRef` is a magic type that doesn't actually appear in any module's source code. In the Haskell land, `JSRef` is first-class and opaque: you can pass it around, put it in a data structure, etc, but under the hood it's just a handle. The runtime maintains mappings from handles to real JavaScript objects.
