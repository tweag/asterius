# Asterius: A Haskell to WebAssembly compiler

[![Docker Pulls](https://img.shields.io/docker/pulls/terrorjack/asterius.svg)](https://hub.docker.com/r/terrorjack/asterius)
[![CircleCI](https://circleci.com/gh/tweag/asterius/tree/master.svg?style=shield)](https://circleci.com/gh/tweag/asterius/tree/master)
[![Gitter](https://img.shields.io/gitter/room/tweag/asterius)](https://gitter.im/tweag/asterius)
[![Netlify Status](https://api.netlify.com/api/v1/badges/e7cfe6ef-b0e6-4a17-bd74-8bce6063f147/deploy-status)](https://asterius.netlify.com)

Asterius is a Haskell to WebAssembly compiler based on GHC. It compiles
Haskell source files or Cabal executable targets to WebAssembly+JavaScript code
which can be run in [Node.js][nodejs] or browsers.
It features seamless JavaScript interop
(lightweight Async FFI with `Promise` support) and small output code (~600KB
`hello.wasm` for a [Hello
World](https://hackage.haskell.org/package/hello-1.0.0.2)). A lot of common
Haskell packages like `lens` are already supported. The project is actively
maintained by [Tweag I/O](https://tweag.io/).

[nodejs]: https://node.js

## Demos

Demos of popular Haskell apps, running in your browser:

* [`ormolu`](https://asterius.netlify.com/ormolu/WebOrmolu.html)
* [`pandoc`](https://asterius.netlify.com/pandoc/pandoc.html)

## Quickstart using the prebuilt Docker image

We host a prebuilt Docker image on [Docker
Hub](https://hub.docker.com/r/terrorjack/asterius). The image also ships ~2k
prebuilt [packages](https://github.com/tweag/asterius/issues/354) from a recent
Stackage snapshot for convenience of testing simple programs without needing to
set up a Cabal project.

To use the image, mount the working directory containing the source code as a
Docker shared volume, then use the `ahc-link` program:

```
username@hostname:~/project$ docker run --rm -it -v $(pwd):/project -w /project terrorjack/asterius
asterius@hostname:/project$ ahc-link --input-hs main.hs
```

There are a lot of link-time options available to `ahc-link`, e.g. targeting
the browser platform instead of `node`, adding extra GHC options or setting
runtime parameters. Check the [documentation](https://asterius.netlify.com/) for
further details.

It's also possible to use `ahc-cabal` as a drop-in replacement of `cabal` to
build a Cabal project. Use `ahc-dist` with `--input-exe` on the output
"executable" file to generate actual WebAssembly and JavaScript artifacts. See
the `diagrams` blog
[post](https://www.tweag.io/posts/2019-12-19-asterius-diagrams.html) for an
example.

Check the official
[reference](https://docs.docker.com/engine/reference/commandline/run) of `docker
run` to learn more about the command given in the example above. The example
opens an interactive `bash` session for exploration, but it's also possible to
use `docker run` to invoke the Asterius compiler on local Haskell source files.
Note that [`podman`](https://podman.io) can be used instead of `docker` here.

## Building and using `asterius` locally

See the [Building guide](https://asterius.netlify.com/building.html) in the
documentation for details.

## Hacking on Asterius

We recommend using [VSCode Remote
Containers](https://code.visualstudio.com/docs/remote/containers) to reproduce
the very same dev environment used by our core team members. The initial
container build will take some while, since it will build the whole project and
run the boot process. After that, the workflow should be pretty smooth.

## Documentation

We have [documentation](https://asterius.netlify.com/) and blog posts:

* [Fibonacci compiles end-to-end: Haskell to WebAssembly via
  GHC](https://www.tweag.io/posts/2018-05-29-hello-asterius.html)
* [Haskell WebAssembly calling JavaScript and back
  again](https://www.tweag.io/posts/2018-09-12-asterius-ffi.html)
* [Asterius GHC WebAssembly backend reaches
  TodoMVC](https://www.tweag.io/posts/2018-12-20-asterius-todomvc.html)
* [Haskell art in your browser with
  Asterius](https://www.tweag.io/posts/2019-12-19-asterius-diagrams.html)

Also checkout the [HIW 2018 lightning
talk](https://icfp18.sigplan.org/details/hiw-2018-papers/6/Lightning-talk-Asterius-Bringing-Haskell-to-WebAssembly),
and the slides of an introductory talk in 2020
[here](https://docs.google.com/presentation/d/1AZJIf2ykheqONOM23oC6F3LJ9m5W9gbl69pDVdZszHg/edit?usp=sharing).

Note that they may be slightly out-of-date as the project evolves. Whenever you
find something in the docs of blog posts which doesn't reflect the status quo,
it's a bug and don't hesitate to open a ticket :)

## What works now

* Almost all GHC language features (TH support is partial, cross-splice state
  persistence doesn't work yet).
* The pure parts in standard libraries and other packages. IO is achieved via
  rts primitives or user-defined JavaScript imports.
* Importing JavaScript expressions via the `foreign import javascript` syntax.
  First-class garbage collected `JSVal` type in Haskell land.
* Preliminary copying GC, managing both Haskell heap objects and JavaScript
  references.
* Preliminary Cabal support.
* Marshaling between Haskell/JavaScript types based on `aeson`.
* Calling Haskell functions from JavaScript via the `foreign export javascript`
  syntax. Haskell closures can be passed between the Haskell/JavaScript boundary
  via `StablePtr`.
* Invoking RTS API on the JavaScript side to manipulate Haskell closures and
  trigger evaluation.
* A linker which performs aggressive dead-code elimination, producing as small
  WebAssembly binary as possible.
* A debugger which checks invalid memory access and outputs memory loads/stores
  and control flow transfers.
* Complete
  [`binaryen`](https://github.com/WebAssembly/binaryen)/[`wabt`](https://github.com/WebAssembly/wabt)
  raw bindings, plus a monadic EDSL to construct WebAssembly code directly in
  Haskell.
* A Haskell library to handle WebAssembly code, which already powers binary code
  generation.
* Besides WebAssembly MVP and `BigInt`, no special requirements on the
  underlying JavaScript engine at the moment.

## Sponsors

[<img src="https://www.tweag.io/img/tweag-med.png" height="65">](https://tweag.io)

Asterius is maintained by [Tweag I/O](https://tweag.io/).

Have questions? Need help? Tweet at [@tweagio](https://twitter.com/tweagio).
