# Asterius: A Haskell to WebAssembly compiler

[![CircleCI](https://circleci.com/gh/tweag/asterius/tree/master.svg?style=shield)](https://circleci.com/gh/tweag/asterius/tree/master)
[![AppVeyor](https://ci.appveyor.com/api/projects/status/github/tweag/asterius?branch=master&svg=true)](https://ci.appveyor.com/project/GHCAppveyor/asterius?branch=master)

A Haskell to WebAssembly compiler. Project status: **alpha**, in active development, some simple examples already work.

See the [documentation](https://tweag.github.io/asterius) for further instructions.

## Quick start

We provide pre-built Docker images. Put the input `.hs` program in a directory and map the directory to a Docker volume:

```
terrorjack@ubuntu:~$ docker run -it -v ~/mirror:/mirror terrorjack/asterius
root@76bcb511663d:~# cd /mirror
root@76bcb511663d:/mirror# ahc-link --help
...
```

See the help text of `ahc-link` for further instructions.

What works for now:

* All GHC language features except Template Haskell
* Non-IO parts in `ghc-prim`/`integer-simple`/`base`
* Importing JavaScript expressions via the `foreign import javascript` syntax

Better check the [`fib`](asterius/test/fib/fib.hs) and [`jsffi`](asterius/test/jsffi/jsffi.hs) test suites first to get some idea on current capabilities of `asterius`.

## Sponsors

[<img src="https://www.tweag.io/img/tweag-med.png" height="65">](https://tweag.io)

Asterius is maintained by [Tweag I/O](https://tweag.io/).

Have questions? Need help? Tweet at [@tweagio](https://twitter.com/tweagio).
