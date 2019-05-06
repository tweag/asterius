# Asterius: A Haskell to WebAssembly compiler

[![CircleCI](https://circleci.com/gh/tweag/asterius/tree/master.svg?style=shield)](https://circleci.com/gh/tweag/asterius/tree/master)
[![Docker Pulls](https://img.shields.io/docker/pulls/terrorjack/asterius.svg)](https://hub.docker.com/r/terrorjack/asterius)
[![Gitter chat](https://badges.gitter.im/tweag/asterius.png)](https://gitter.im/tweag/asterius)

A Haskell to WebAssembly compiler. Project status: **alpha**, in active development, some simple examples already work.

See the [documentation](https://tweag.github.io/asterius) for further instructions. Or check our blog posts:

* [Fibonacci compiles end-to-end: Haskell to WebAssembly via GHC](https://www.tweag.io/posts/2018-05-29-hello-asterius.html)
* [Haskell WebAssembly calling JavaScript and back again](https://www.tweag.io/posts/2018-09-12-asterius-ffi.html)
* [Asterius GHC WebAssembly backend reaches TodoMVC](https://www.tweag.io/posts/2018-12-20-asterius-todomvc.html)

Also, we've added [Weekly Status Reports](https://tweag.github.io/asterius/reports) in case you're interested where the bleeding edge has reached.

## Quick start

We provide pre-built Docker images. Put the input `.hs` program in a directory and map the directory to a Docker volume:

```
terrorjack@ubuntu:~$ docker run -it -v ~/mirror:/mirror terrorjack/asterius
root@76bcb511663d:~# cd /mirror
root@76bcb511663d:/mirror# ahc-link --input-hs xxx.hs
...
```

###### To run the sameple `TODOMVC` which comes bundled:

We spawn the Docker image, use `Asterius` to build the app which comes bundled,
copy it to the `mirror` directory and serve it.

```
terrorjack@ubuntu:~$ docker run -it -v ~/mirror:/mirror terrorjack/asterius

(Now inside docker image)
root@76bcb511663d:~# cd /mirror
root@004f361854e2:/mirror# ahc-link --input-hs=/root/asterius/asterius/test/todomvc/todomvc.hs --browser
[INFO] Compiling /root/asterius/asterius/test/todomvc/todomvc.hs to WebAssembly
[INFO] Converting linked IR to wasm-toolkit IR
[INFO] Writing WebAssembly binary to "/root/asterius/asterius/test/todomvc/todomvc.wasm"
[INFO] Writing JavaScript runtime settings to "/root/asterius/asterius/test/todomvc/rts.settings.mjs"
[INFO] Writing JavaScript runtime modules to "/root/asterius/asterius/test/todomvc"
[INFO] Writing JavaScript loader module to "/root/asterius/asterius/test/todomvc/todomvc.wasm.mjs"
[INFO] Writing JavaScript lib module to "/root/asterius/asterius/test/todomvc/todomvc.lib.mjs"
[INFO] Writing JavaScript entry module to "/root/asterius/asterius/test/todomvc/todomvc.mjs"
[INFO] Writing HTML to "/root/asterius/asterius/test/todomvc/todomvc.html"

root@004f361854e2:/mirror# cp -r /root/asterius/asterius/todomvc .

(From the outside)
user@machine:~# cd ~/mirror; python3 -m http.server


(Now navigate to http://localhost:8000 to view the TODOMVC app)
```


See the [help text](https://tweag.github.io/asterius/ahc-link) of `ahc-link` for further instructions.

What works currently:

* All GHC language features except Template Haskell.
* Non-IO parts in `ghc-prim`/`integer-simple`/`base`/`array`/`deepseq`/`containers`/`transformers`/`mtl`/`pretty`/`bytestring`/`binary`/`xhtml`. IO is achieved via rts primitives like `print_i64` or JavaScript FFI.
* Fast arbitrary-precision `Integer` operations backed by `BigInt`s.
* Preliminary copying GC, managing both Haskell heap objects and JavaScript references.
* Preliminary Cabal support.
* Importing JavaScript expressions via the `foreign import javascript` syntax. First-class `JSVal` type in Haskell land.
* Fast conversion between Haskell/JavaScript types (strings, arrays and ArrayBuffers at the moment)
* Calling Haskell functions from JavaScript via the `foreign export javascript` syntax. Haskell closures can be passed between Haskell/JavaScript boundary via `StablePtr`.
* Invoking RTS API on the JavaScript side to manipulate Haskell closures and trigger evaluation.
* A linker which performs aggressive dead-code elimination, producing as small WebAssembly binary as possible.
* A debugger which checks invalid memory access and outputs memory loads/stores and control flow transfers.
* Complete [`binaryen`](https://github.com/WebAssembly/binaryen)/[`wabt`](https://github.com/WebAssembly/wabt) raw bindings, plus a monadic EDSL to construct WebAssembly code directly in Haskell.
* A Haskell library to handle WebAssembly code, which already powers binary code generation.
* Unit tests implementing stochastic fuzzer/shrinker for WebAssembly, in order to produce minimal repro in case something goes wrong in generated code.
* Besides WebAssembly MVP and `BigInt`, no special requirements on the underlying JavaScript engine at the moment. Optionally, we emit binaries using the experimental tail call opcodes; see the `ahc-link` documentation page for details.

Better check the [`fib`](asterius/test/fib/fib.hs), [`jsffi`](asterius/test/jsffi/jsffi.hs), [`array`](asterius/test/array/array.hs), [`rtsapi`](asterius/test/rtsapi.hs) and [`teletype`](asterius/test/teletype/teletype.hs) test suites first to get some idea on current capabilities of `asterius`.

## Sponsors

[<img src="https://www.tweag.io/img/tweag-med.png" height="65">](https://tweag.io)

Asterius is maintained by [Tweag I/O](https://tweag.io/).

Have questions? Need help? Tweet at [@tweagio](https://twitter.com/tweagio).
