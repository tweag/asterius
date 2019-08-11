# Using experimental WebAssembly features

By default, asterius only emits code that uses WebAssembly MVP features. There
are [flags](ahc-link.md) to make use of WebAssembly experimental features:

* `--tail-calls`: Emits tail call opcodes for Cmm function calls; overrides the
  default trampoline approach. Only supported by the `wasm-toolkit` backend at
  the moment.
* `--debug`: Uses i64 BigInt integration for passing i64 values between js/wasm.

The above features require specific flags to switch on in V8. They are known to
work in latest Node.js 12.x versions, and we test them on CI.

The V8 team maintains a Node.js 13.x build which integrates V8 trunk, described
[here](https://v8.dev/docs/node-next-generation). It's possible to use that
build to evaluate experimental WebAssembly features; we provide a
[script](https://github.com/tweag/asterius/blob/master/utils/v8-node.py) which
unzips the latest test-passing build to the current directory, so it's possible
to use the `node` binary for testing bleeding-edge wasm features in V8.

We are keeping an eye on the development of experimental WebAssembly features.
Here is a list of V8 tracking issues of the features we are interested in. Some
are already available in recent Node.js or Chromium releases.

* [WebAssembly SIMD](https://bugs.chromium.org/p/v8/issues/detail?id=6020)
* [WebAssembly Multi-value](https://bugs.chromium.org/p/v8/issues/detail?id=6672)
* [WebAssembly nontrapping float-to-int conversions](https://bugs.chromium.org/p/v8/issues/detail?id=7226)
* [Tail call opcodes](https://bugs.chromium.org/p/v8/issues/detail?id=7431)
* [Reference types](https://bugs.chromium.org/p/v8/issues/detail?id=7581)
* [WebAssembly i64 BigInt integration](https://bugs.chromium.org/p/v8/issues/detail?id=7741)
* [WebAssembly JS Reflection API](https://bugs.chromium.org/p/v8/issues/detail?id=7742)
* [WebAssembly Bulk Memory](https://bugs.chromium.org/p/v8/issues/detail?id=7747)
* [Garbage Collection](https://bugs.chromium.org/p/v8/issues/detail?id=7748)
* [Exception handling](https://bugs.chromium.org/p/v8/issues/detail?id=8091)
