# Hacking guide

## Hacking with `ghcid`

A known-to-work workflow of hacking asterius is using `ghcid`. We also include
an example `.ghcid` file, so running `ghcid` at the project root directory shall
work out of the box.

Some notes regarding the usage of `ghcid`:

* Multiple lib targets can be loaded at once, but only one main target
  (exe/test) can be loaded. When hacking a specific exe/test, modify the local
  `.ghcid` file first, and don't commit the change. And before commiting, it
  would be nice to run `stack build --test --no-run-tests` to make sure all
  executables are up-to-date and not broken by lib changes.

* If some weird linker-related error related to `ghc-toolkit` or `binaryen` pops
  up when loading `ghcid`, try adding `ghc-toolkit:lib` or `binaryen:lib` to one
  of the `ghcid` targets in `.ghcid`.

## Boot cache maintainence

As described in the building guide, `stack build` only builds the asterius
compiler itself; additionally we need to run `stack exec ahc-boot` to run the
compiler on the boot libs. This process is typically only needed once, but there
are cases when it needs to be re-run:

* The boot libs in `ghc-toolkit/boot-libs` are modified.
* The `Asterius.Types` module is modified, so the IR types have changed.
* The `Asterius.CodeGen` module is modified and you're sure different code will
  be generated when compiling the same Haskell/Cmm files.

Most other modifications in the asterius lib/exes won't need a reboot.
Specifically:

* `Asterius.Builtins` modifications don't impact the boot cache. The builtin
  module is generated on the fly with every linker invocation.

When rebooting, run `utils/clean.sh` in the project root directory to purge the
cache, then rerun `stack build` and `stack exec ahc-boot`.

The `ahc-boot` process is configurable via these environment variables:

* `ASTERIUS_CONFIGURE_OPTIONS`
* `ASTERIUS_BUILD_OPTIONS`
* `ASTERIUS_INSTALL_OPTIONS`

A common usage is setting `ASTERIUS_BUILD_OPTIONS=-j8` to enable parallelism in
booting, reducing your coffee break time.

## Adding a test case

To add a test case, it is best to replicate what has been done for an existing testcase. 

- For example, `git grep bytearraymini` should show all the places where the test case
`bytearraymini` has been used. Replicating the same files for a new test case
should "just work".
