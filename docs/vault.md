# The "Vault"

Asterius provides a "persistent vault" feature, which provides a KV store per asterius instance, and the store can be accessed in both Haskell and JavaScript. The vault enables compiled Haskell code to reuse some state, even if the whole asterius instance is wiped and restarted. See GitHub issue [48](https://github.com/tweag/asterius/issues/48) for further explanation.

The Haskell API is in `Asterius.Vault` in `base`:

```
vaultInsert :: JSArrayBuffer -> JSVal -> IO ()
vaultLookup :: JSArrayBuffer -> IO (Maybe JSVal)
vaultDelete :: JSArrayBuffer -> IO ()
```

The key of a vault is a `JSArrayBuffer`, typically converted from a `ByteString`. The value can be `JSVal`, which can be `coerce`ed from any `JS*` type.

In JavaScript, assuming `i` is the asterius instance, then `i.vault` is the instance vault. `i.vault` defaults to empty, and can be passed around, modified and assigned.

The `i.vault` value is a `Map` object which uses immutable `String`s converted from `ArrayBuffer`s as keys. It's only safe to manipulate keys in JavaScript when you're sure the strings only encode Latin-1 characters.
