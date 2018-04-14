`asterius` is tested on Linux x64 and Windows x64. macOS x64 may also work.

tl;dr: See [`.circleci/config.yml`](https://github.com/tweag/asterius/blob/master/.circleci/config.yml) for CircleCI config, [`appveyor.yml`](https://github.com/tweag/asterius/blob/master/appveyor.yml) for AppVeyor config.

## Building custom `ghc`

`asterius` requires a custom `ghc` which:

* Uses `ghc-head` instead of a release version. It forces us to keep an eye on upstream changes. The `master` branch of `ghc` may introduce breaking commits, so for safety you should choose the specific `ghc` commit as indicated in the `NIXPKGS_REV` revision of the CircleCI config file, details below.
* Disables [`TABLES_NEXT_TO_CODE`](https://ghc.haskell.org/trac/ghc/wiki/Commentary/Rts/Storage/HeapObjects#TABLES_NEXT_TO_CODE). It's impossible to attach executable code to an info table on the WebAssembly platform.
* Uses `integer-simple` instead of `integer-gmp`. Porting `integer-gmp` to WebAssembly requires extra work and is not currently scheduled.

The building guide of `ghc` can be found [here](https://ghc.haskell.org/trac/ghc/wiki/Building). Add the following lines to `mk/build.mk`:

```
GhcEnableTablesNextToCode = NO
INTEGER_LIBRARY           = integer-simple
```

In addition to your own build configs.

On Linux, a `nix` binary cache for the custom `ghc` is provided. See the CircleCI config file at `.circleci/config.yml` for details. The `NIXPKGS_REV` environment variable specifies which revision of [patched `nixpkgs`](https://github.com/TerrorJack/asterius-nixpkgs) should be used. The `haskell.compiler.ghcAsterius` package is the custom `ghc` for `asterius`. The binary cache server address is `https://canis.sapiens.moe/` (signatures not enabled yet).

On Windows, a prebuilt `ghc` tarball is provided. It's already included in [`stack.yaml`](https://github.com/tweag/asterius/blob/master/stack.yaml).

## Extra dependencies

Besides the custom `ghc`, these dependencies are also required:

* `cmake`/`make`/`g++`: For building in-tree [`binaryen`](https://github.com/WebAssembly/binaryen)
* `autoconf`: For booting `ghc-prim`/`base`
* `nodejs`: For running tests
* `stack`

## Building `asterius`

`stack build asterius`. That's it. If you are using `nix`, make sure to add `--system-ghc` when invoking `stack`.

Set `MAKEFLAGS=-j8` to pass flags to `make` for parallel building of `binaryen`. Run `stack exec ahc-boot` to test if booting works.
