# Asterius: A Haskell to WebAssembly compiler

[![Docker Pulls](https://img.shields.io/docker/pulls/terrorjack/asterius.svg)](https://hub.docker.com/r/terrorjack/asterius)
![](https://github.com/tweag/asterius/workflows/pipeline/badge.svg?branch=master)
[![Gitter](https://img.shields.io/gitter/room/tweag/asterius)](https://gitter.im/tweag/asterius)
[![Netlify Status](https://api.netlify.com/api/v1/badges/e7cfe6ef-b0e6-4a17-bd74-8bce6063f147/deploy-status)](https://asterius.netlify.app)

Asterius is a Haskell to WebAssembly compiler based on GHC. It compiles
Haskell source files or Cabal executable targets to WebAssembly+JavaScript code
which can be run in [Node.js][nodejs] or browsers.
It features seamless JavaScript interop
(lightweight Async FFI with `Promise` support) and small output code (~600KB
`hello.wasm` for a [Hello
World](https://hackage.haskell.org/package/hello-1.0.0.2)). A lot of common
Haskell packages like `lens` are already supported. The project is actively
maintained by [Tweag I/O](https://tweag.io/).

[nodejs]: https://nodejs.org

## Demos

Demos of popular Haskell apps, running in your browser:

- [`ormolu`](https://asterius.netlify.app/ormolu/WebOrmolu.html)
- [`pandoc`](https://asterius.netlify.app/pandoc/pandoc.html)

## Quickstart using the prebuilt container image

We host a prebuilt container image on [Docker
Hub](https://hub.docker.com/r/terrorjack/asterius). The image also ships ~2k
prebuilt [packages](https://github.com/tweag/asterius/issues/354) from a recent
Stackage snapshot for convenience of testing simple programs without needing to
set up a Cabal project.

To use the image, mount the working directory containing the source code as a
shared volume, then use the `ahc-link` program:

```console
terrorjack@hostname:/project$ podman run -it --rm -v $(pwd):/workspace -w /workspace terrorjack/asterius
root@hostname:/workspace#
```

There are a lot of link-time options available to `ahc-link`, e.g. targeting
the browser platform instead of `node`, adding extra GHC options or setting
runtime parameters. Check the [documentation](https://asterius.netlify.app/) for
further details.

It's also possible to use `ahc-cabal` as a drop-in replacement of `cabal` to
build a Cabal project. Use `ahc-dist` with `--input-exe` on the output
"executable" file to generate actual WebAssembly and JavaScript artifacts. See
the `diagrams` blog
[post](https://www.tweag.io/posts/2019-12-19-asterius-diagrams.html) for an
example.

Check the documentation [section](https://asterius.netlify.app/images.html)
about the prebuilt image for more information, e.g. versioning policy, how to
use with `podman`/`docker`, etc.

## Building and using `asterius` locally

See the [Building guide](https://asterius.netlify.app/building.html) in the
documentation for details.

## Hacking on Asterius

We recommend using [VSCode Remote
Containers](https://code.visualstudio.com/docs/remote/containers) to reproduce
the very same dev environment used by our core team members. See the [Hacking
guide](https://asterius.netlify.app/hacking.html) in the documentation for
details.

## Documentation

We have [documentation](https://asterius.netlify.app/) and blog posts:

- [Fibonacci compiles end-to-end: Haskell to WebAssembly via
  GHC](https://www.tweag.io/posts/2018-05-29-hello-asterius.html)
- [Haskell WebAssembly calling JavaScript and back
  again](https://www.tweag.io/posts/2018-09-12-asterius-ffi.html)
- [Asterius GHC WebAssembly backend reaches
  TodoMVC](https://www.tweag.io/posts/2018-12-20-asterius-todomvc.html)
- [Haskell art in your browser with
  Asterius](https://www.tweag.io/posts/2019-12-19-asterius-diagrams.html)

Also checkout the [HIW 2018 lightning
talk](https://icfp18.sigplan.org/details/hiw-2018-papers/6/Lightning-talk-Asterius-Bringing-Haskell-to-WebAssembly),
and the slides of an introductory talk in 2020
[here](https://docs.google.com/presentation/d/1AZJIf2ykheqONOM23oC6F3LJ9m5W9gbl69pDVdZszHg/edit?usp=sharing).

Note that they may be slightly out-of-date as the project evolves. Whenever you
find something in the docs of blog posts which doesn't reflect the status quo,
it's a bug and don't hesitate to open a ticket :)

## Project status & roadmap

See the [roadmap](https://asterius.netlify.app/roadmap.html) section in the
documentation for details.

## Contributors

&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;
[<img src="https://tweag.io/logo.png" height="65">](https://tweag.io)
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;
[<img src="https://i.imgur.com/tAag5MD.jpg" height="65">](https://iohk.io)

Asterius is maintained by [Tweag I/O](https://tweag.io/).

Have questions? Need help? Tweet at [@tweagio](https://twitter.com/tweagio).
