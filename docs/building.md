# Building guide

`asterius` is tested on Linux x64 and Windows x64. macOS x64 may also work. A Docker image is provided.

tl;dr: See [`.circleci/config.yml`](https://github.com/tweag/asterius/blob/master/.circleci/config.yml) for CircleCI config, [`appveyor.yml`](https://github.com/tweag/asterius/blob/master/appveyor.yml) for AppVeyor config.

## Using a pre-built Docker image

We build and test Docker images on CircleCI. They are pushed to `terrorjack/asterius`, the tags are git revisions. `terrorjack/asterius:latest` correspond to latest revision on `master`.

Put input program in a directory (e.g. `~/mirror`), then map the directory to a Docker volume:

```
terrorjack@ubuntu:~$ docker run -it -v ~/mirror:/mirror terrorjack/asterius
root@76bcb511663d:~# cd /mirror
root@76bcb511663d:/mirror# ahc-link --help
...
```

## Building custom `ghc`

`asterius` requires a custom `ghc` which:

* Uses `ghc-head` instead of a release version. It forces us to keep an eye on upstream changes. The `master` branch of `ghc` may introduce breaking commits, so for safety you should choose the specific `ghc` version as indicated in the `stack.yaml` file.
* Integrates [D5079](https://phabricator.haskell.org/D5079).

The building guide of `ghc` can be found [here](https://ghc.haskell.org/trac/ghc/wiki/Building).

In addition to your own build configs.

On Linux/Windows, a prebuilt `ghc` tarball is provided. It's already included in [`stack.yaml`](https://github.com/tweag/asterius/blob/master/stack.yaml). Note that the Windows bindist does not provide prof libs/haddock (due to AppVeyor build time restriction).

## Extra dependencies

Besides the custom `ghc`, these dependencies are also required:

* `cmake`/`make`/`g++`: For building in-tree [`binaryen`](https://github.com/WebAssembly/binaryen)
* `autoconf`: For booting `ghc-prim`/`base`
* `nodejs`: For running tests. Ensure the latest version is used, since we rely on some recent V8 experimental features (e.g. BigInt support)
* `stack`: Someday `cabal` may also work, no specific obstacles anyway.

## Building `asterius`

`stack build asterius`. That's it. Set `MAKEFLAGS=-j8` to pass flags to `make` for parallel building of `binaryen`.

After the dust settles, run `stack exec ahc-boot` to perform booting. Set the `ASTERIUS_DEBUG` environment variable to make `ahc-boot` pretty-print IRs to text files which are useful when debugging compiled code of standard libraries. Be aware that this flag slows down the booting process significantly!
