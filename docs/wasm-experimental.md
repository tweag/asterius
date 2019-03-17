# Using experimental WebAssembly features

Currently, asterius only emits code that uses WebAssembly MVP features. At this moment, multiple WebAssembly feature proposals are actively being discussed and developed, and we are keeping an eye on them.

The V8 team maintains a Node.js build which integrates V8 trunk, described [here](https://v8.dev/docs/node-next-generation). It's possible to use that build to evaluate experimental WebAssembly features; we provide `utils/v8-node.py` which unzips the latest test-passing build to the current directory.

Here is a list of V8 tracking issues of the features we are interested in. Some are already available in recent Node.js or Chromium releases.

* [WebAssembly SIMD](https://bugs.chromium.org/p/v8/issues/detail?id=6020)
* [WebAssembly Multi-value](https://bugs.chromium.org/p/v8/issues/detail?id=6672)
* [WebAssembly nontrapping float-to-int conversions](https://bugs.chromium.org/p/v8/issues/detail?id=7226)
* [Tail call opcodes](https://bugs.chromium.org/p/v8/issues/detail?id=7431)
* [Reference types](https://bugs.chromium.org/p/v8/issues/detail?id=7581)
* [WebAssembly i64 BigInt integration](https://bugs.chromium.org/p/v8/issues/detail?id=7741)
* [WebAssembly JS Reflection API](https://bugs.chromium.org/p/v8/issues/detail?id=7742)
* [WebAssembly Bulk Memory](https://bugs.chromium.org/p/v8/issues/detail?id=7747)
* [Exception handling support](https://bugs.chromium.org/p/v8/issues/detail?id=8091)
* [Garbage Collection](https://bugs.chromium.org/p/v8/issues/detail?id=8217)
