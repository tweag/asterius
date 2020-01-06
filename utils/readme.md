# Util scripts

This directory contains util scripts for hacking on asterius.

## Shell scripts

These scripts must be called at the project root directory.

* `utils/clean.sh`: Clean up the `ghc-toolkit` and `asterius` packages, removing
  the source & compiled objects of the boot libs.
* `utils/coffee-break.sh`: Do the cleanup, rebuild and reboot, using all CPU
  cores.
* `utils/noon-break.sh`: Similar to coffee break, but disables parallelism when
  booting for deterministic ABI, and enables `ASTERIUS_DEBUG` for IR dumps and
  Core/STG/Cmm linting. Natually, this one takes a lot of more time.
* `utils/stack-ide.sh`: Run `stack build` with the `--file-watch` option to get
  real time GHC compilation error feedback when working on the Haskell sources
  of `asterius`.

In general, when the boot libs source or the wasm codegen logic is changed, the
existing boot cache becomes out of date and thus must be rebuilt. Pick
`coffee-break.sh` or `noon-break.sh` and get some rest then :)

## Formatting IR dumps with `pretty-show`

`utils/format.hs` is a script to format IR dumps with `pretty-show`. It's used
in very early days of this project and most likely bit rotten; open an issue to
fix this script when the IR dumps need to be eyeballed again.
