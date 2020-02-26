# Asterius: A Haskell to WebAssembly compiler

[![Docker Pulls](https://img.shields.io/docker/pulls/terrorjack/asterius.svg)](https://hub.docker.com/r/terrorjack/asterius)
[![CircleCI](https://circleci.com/gh/tweag/asterius/tree/master.svg?style=shield)](https://circleci.com/gh/tweag/asterius/tree/master)
[![Gitter](https://img.shields.io/gitter/room/tweag/asterius)](https://gitter.im/tweag/asterius)
[![Netlify Status](https://api.netlify.com/api/v1/badges/e7cfe6ef-b0e6-4a17-bd74-8bce6063f147/deploy-status)](https://asterius.netlify.com)

Asterius is a Haskell to WebAssembly compiler based on GHC. It compiles simple
Haskell source files or Cabal executable targets to WebAssembly+JavaScript code
which can be run in node.js or browsers. It features seamless JavaScript interop
(lightweight Async FFI with `Promise` support) and small output code (~600KB
`hello.wasm` for a [Hello
World](https://hackage.haskell.org/package/hello-1.0.0.2)). A lot of common
Haskell packages like `lens` are already supported. The project is actively
maintained by [Tweag I/O](https://tweag.io/).

## Quickstart using the pre-built Docker image

We host a pre-built Docker image on [Docker
Hub](https://hub.docker.com/r/terrorjack/asterius). The image also ships ~2k
pre-built [packages](https://github.com/tweag/asterius/issues/354) from a recent
Stackage snapshot for convenience of testing simple programs without needing to
set up a Cabal project.

To use the image, mount the working directory containing the source code as a
Docker shared volume and use the `ahc-link` program:

```
username@hostname:~/project$ docker run --rm -it -v $(pwd):/project -w /project terrorjack/asterius
asterius@hostname:/project$ ahc-link --input-hs main.hs
```

There are a lot of link-time options available to `ahc-link`, e.g. targetting
the browser platform instead of `node`, adding extra GHC options or setting
runtime parameters. Check the [documentation](https://asterius.netlify.com/) for
further details.

It's also possible to use `ahc-cabal` as a drop-in replacement of `cabal` to
build your Cabal project. Use `ahc-dist` with `--input-exe` on the output
"executable" file to generate actual WebAssembly and JavaScript artifacts. See
the `diagrams` blog
[post](https://www.tweag.io/posts/2019-12-19-asterius-diagrams.html) for an
example.

## Building and using `asterius` locally

`asterius` is a regular `stack` project which relies on a custom GHC fork.
Pre-built GHC bindists are available for `linux64` and `macosx`. Simply use a
regular `stack build asterius` for building it, and `stack exec ahc-boot` to
boot the standard libraries, so later `stack exec ahc-link` may work.

In addition to regular GHC dependencies, make sure these dependencies are
present in your environment:

* `libnuma-dev` (Required by GHC)
* `cmake`, `g++`, `git`, `python3` (Required by `binaryen`)
* `automake`, `autoconf` (Required by `ahc-boot`)
* `node` (`v12` or later)

If you use `direnv`, after doing a `stack build asterius`, you can directly use
`ahc-boot` or `ahc-link` without `stack exec` in the project directory.

## Documentation and blog posts

We have [documentation](https://asterius.netlify.com/) and blog posts:

* [Fibonacci compiles end-to-end: Haskell to WebAssembly via
  GHC](https://www.tweag.io/posts/2018-05-29-hello-asterius.html)
* [Haskell WebAssembly calling JavaScript and back
  again](https://www.tweag.io/posts/2018-09-12-asterius-ffi.html)
* [Asterius GHC WebAssembly backend reaches
  TodoMVC](https://www.tweag.io/posts/2018-12-20-asterius-todomvc.html)
* [Haskell art in your browser with
  Asterius](https://www.tweag.io/posts/2019-12-19-asterius-diagrams.html)

Note that they may be slightly out-of-date as the project evolves. Whenever you
find something in the docs of blog posts which doesn't reflect the status quo,
don't hesitate to open a ticket :)

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
