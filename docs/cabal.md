# Cabal support

Asterius now has preliminary Cabal support. By substituting toolchain
executables like `ghc`/`ghc-pkg` and supplying some other configure options,
Cabal can build static libraries and "executables" using asterius. The
"executables" can be quickly converted to node/web artifacts using `ahc-dist`.

We also provide `ahc-cabal` which is a wrapper for `cabal`. `ahc-cabal` works
with typical nix-style commands like `new-update`/`new-build`, etc. The legacy
commands with `v1` prefix may also work.
