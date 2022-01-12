[![Docker Pulls](https://img.shields.io/docker/pulls/terrorjack/asterius.svg)](https://hub.docker.com/r/terrorjack/asterius)
![](https://github.com/tweag/asterius/workflows/pipeline/badge.svg?branch=master)
[![Gitter](https://img.shields.io/gitter/room/tweag/asterius)](https://gitter.im/tweag/asterius)
[![Netlify Status](https://api.netlify.com/api/v1/badges/e7cfe6ef-b0e6-4a17-bd74-8bce6063f147/deploy-status)](https://asterius.netlify.app)

Asterius is a Haskell to WebAssembly compiler based on GHC. It
compiles simple Haskell source files or Cabal executable targets to
WebAssembly+JavaScript code which can be run in node.js or browsers.
It features seamless JavaScript interop (lightweight Async FFI with
`Promise` support) and small output code (~600KB `hello.wasm` for a
[Hello World](https://hackage.haskell.org/package/hello-1.0.0.2)). A
lot of common Haskell packages like `lens` are already supported. The
project is actively maintained by [Tweag I/O](https://tweag.io/).

# Contributors

[<img src="./tweag-logo.svg" height="65">](https://tweag.io)

[<img src="https://i.imgur.com/tAag5MD.jpg" height="65">](https://iohk.io)

Asterius is maintained by [Tweag I/O](https://tweag.io/).

Have questions? Need help? Tweet at [@tweagio](https://twitter.com/tweagio).
