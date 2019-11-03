# Util scripts

This directory contains util scripts for hacking on asterius.

## Cleaning & rebuilding

These scripts must be called at the project root directory.

* `utils/clean.sh`: Clean up the `ghc-toolkit` and `asterius` packages, removing
  the source & compiled objects of the boot libs.
* `utils/coffee-break.sh`: Do the cleanup, rebuild and reboot, using all CPU
  cores.
* `utils/noon-break.sh`: Similar to coffee break, but disables parallelism when
  booting for deterministic ABI, and enables `ASTERIUS_DEBUG` for IR dumps and
  Core/STG/Cmm linting. Natually, this one takes a lot of more time.

In general, when the boot libs source or the wasm codegen logic is changed, the
existing boot cache becomes out of date and thus must be rebuilt. Pick
`coffee-break.sh` or `noon-break.sh` and get some rest then :)

## Fetching V8 team's Node.js integration build

`utils/v8-node.py` fetches the V8 team's Node.js integration build described
[here](https://v8.dev/docs/node-integration). The script extracts to
`$(pwd)/bin/node`, which is a Node.js v13 pre-release build with the latest V8
revision. This is useful for testing bleeding edge V8 wasm features.

Note that the V8 team's build doesn't bundle `npm`.

## Formatting IR dumps with `pretty-show`

`utils/format.hs` is a script to format IR dumps with `pretty-show`. It's used
in very early days of this project and most likely bit rotten; open an issue to
fix this script when the IR dumps need to be eyeballed again.
