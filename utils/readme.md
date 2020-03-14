# Util scripts

This directory contains util scripts for hacking on asterius.

## Shell scripts

These scripts are meant to be called at the project root directory.

* `utils/clean.sh`: Clean up the `ghc-toolkit` and `asterius` packages, removing
  the source & compiled objects of the boot libs.
* `utils/ghcid.sh`: Call `ghcid` on `asterius`. Modify the script to add the
  source of an executable/test target when needed. This doesn't require a
  previous boot, but does require a `stack build` of `asterius`, even if it
  fails midway, since it treats the auto-generated `Paths_asterius.hs` as an
  input source file as well.
* `utils/reboot.sh`: Do the cleanup, rebuild and reboot, using all CPU cores. If
  you touch the source of boot libs in `ghc-toolkit/` or modify the IR/codegen
  of `asterius`, the boot cache may be out-of-sync so you need to run this
  script. Set the `ASTERIUS_DEBUG=1` environment variable to enable the IR
  dumps.

## Formatting IR dumps with `pretty-show`

`utils/format.hs` is a script to format IR dumps with `pretty-show`. It's used
in very early days of this project and most likely bit rotten; open an issue to
fix this script when the IR dumps need to be eyeballed again.
