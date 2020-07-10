# Util scripts

This directory contains util scripts for hacking on asterius.

## Shell scripts

These scripts are meant to be called at the project root directory.

* `utils/clean.sh`: Clean up the `ghc-toolkit` and `asterius` packages, removing
  the source & compiled objects of the boot libs.
* `utils/ghcid.sh`: The script used by `ghcid` for `asterius`; do not run it
  directly, instead just run `ghcid` from the project root directory. Modify the
  script to add the source of an executable/test target when needed. This
  doesn't require a previous boot, but does require a `stack build` of
  `asterius`, even if it fails midway.
* `utils/pkgdb.sh`: Source it under the current shell. It sets up the
  `ahc-install` alias which can install packages into the global package
  database, so that later the package can be used by `ahc-link` directly.
* `utils/reboot.sh`: Do the cleanup, rebuild and reboot, using all CPU cores. If
  you touch the source of boot libs in `ghc-toolkit/` or modify the IR/codegen
  of `asterius`, the boot cache may be out-of-sync so you need to run this
  script. Set the `ASTERIUS_DEBUG=1` environment variable to enable the IR
  dumps.
