# Hacking guide

## Using VSCode remote containers

We recommend using [VSCode Remote
Containers](https://code.visualstudio.com/docs/remote/containers) to reproduce
the very same dev environment used by our core team members. The steps to set up
the dev environment are:

* Do a local clone of the asterius repo
* Install VSCode (at least `1.45`) and its remote
  [extension](https://aka.ms/vscode-remote/download/extension)
* Install `podman`, and make sure the `podman` command works with the current user
* Set up a `docker` symlink which points to `podman`, according to VSCode
  [announcement](https://github.com/microsoft/vscode-docs/blob/master/remote-release-notes/v1_45.md#podman-support)
  of `podman` support
* `docker pull terrorjack/asterius:dev`
* Open the asterius repo with remote containers

Opening the repo with remote containers for the first time will take some time,
since it runs the build script to build `asterius` and perform booting. Later
re-opening will be near instant, since it reuses the previous container.

The dev image shall work with `docker` too if the `userns-remap` related
settings are correctly set up. Check the documentation [section](images.md) for
relevant explanation; when using `docker` with default settings, there is a file
permission issue when mounting your local filesystem into the prebuilt container
images.

## Using `direnv`

If `direnv` is enabled, the `PATH` of the current shell session will be extended
to include the locations of Asterius executables. This means it's possible to
run `ahc-link ..` instead of `stack exec ahc-link -- ..`.

## Hacking with `ghcid`

A known-to-work workflow of hacking Asterius is using `ghcid`. We also include
an example `.ghcid` file, so running `ghcid` at the project root directory shall
work out of the box.

Some notes regarding the usage of `ghcid`:

* Multiple lib targets can be loaded at once, but only one main target
  (exe/test) can be loaded. When hacking a specific exe/test, modify the local
  `utils/ghcid.sh` script first. Before committing changes in the Haskell
  codebase, it would be nice to run `stack build --test --no-run-tests` to make
  sure all executables are not broken by lib changes.

## To boot or not to boot

As described in the building guide, `stack build` only builds the Asterius
compiler itself; additionally we need to run `stack exec ahc-boot` to run the
compiler on the boot libs. This process is typically only needed once, but there
are cases when it needs to be re-run:

* The boot libs in `ghc-toolkit/boot-libs` are modified.
* The `Asterius.Types` module is modified, so the IR types have changed.
* The `Asterius.CodeGen` module is modified and you're sure different code will
  be generated when compiling the same Haskell/Cmm files.

Most other modifications in the Asterius lib/exes won't need a reboot.
Specifically:

* `Asterius.Builtins` modifications don't impact the boot cache. The builtin
  module is generated on the fly with every linker invocation.

When rebooting, run `utils/reboot.sh` in the project root directory, so that we
can ensure the booting is used with the up-to-date version of `asterius` and the
boot lib sources.

The `ahc-boot` process is configurable via these environment variables:

* `ASTERIUS_CONFIGURE_OPTIONS`
* `ASTERIUS_BUILD_OPTIONS`
* `ASTERIUS_INSTALL_OPTIONS`

## Doing profiled builds

### Doing profiled builds within a local git tree

Use `stack-profile.yaml` to overwrite `stack.yaml`, and then run
`utils/reboot.sh` to kick off the rebooting process. This will be quite slow due
to the nature of profiled builds; all libraries will be rebuilt with the
profiled flavor. Better to perform a profiled build in a standalone git tree.

Once the profiled build is complete, it's possible to use RTS flags to obtain
profile data when compiling Haskell sources. At runtime there are two ways to
pass RTS flags to a Haskell executable:

* The `GHCRTS` environment variable
* The `+RTS ... -RTS` command line arguments

Always use `GHCRTS` when running programs like `ahc-link`, since those programs
can spawn other processes (e.g. `ahc-ld`), and we're often interested in the
profile data of all Asterius executables. The `GHCRTS` environment variable can
propagate to all processes.

See the relevant
[section](https://downloads.haskell.org/~ghc/8.8.3/docs/html/users_guide/profiling.html)
in the GHC user guide for more information on profiling Haskell apps. There are
also some third party applications useful for analyzing the profiling data, e.g.
[`eventlog2html`](https://github.com/mpickering/eventlog2html),
[`ghc-prof-flamegraph`](https://github.com/fpco/ghc-prof-flamegraph).

Fow now, a major problem with the profiled build is: it seems to emit
dysfunctional code which doesn't work. Consequently, this affects the TH runner,
so any dependencies relying on TH isn't supported by the profiled build.

### Measuring time/allocation differences

When working on a performance-related PR, we often want to measure the
time/allocation differences it introduced. The workflow is roughly:

* Perform *two* profiled builds with Docker; one builds from the `master`
  branch, one from the PR's branch.
* Run `ahc-link` in the built images on the example program below, setting the
  necessary `GHCRTS` to generate the profile reports. The code should be put in
  two standalone directories, otherwise the `.hi`/`.o` files may conflict or be
  accidentally reused.

The profiled Docker images contain pre-compiled `Cabal`. And the example program
we use to stress-test the linker is:

```haskell
import Distribution.Simple
main = defaultMain
```

We choose this program since it's classic, and although being short, it pulls in
a lot of data segments and functions, so it exposes the linker's performance
bottleneck pretty well.

## Adding a test case

To add a test case, it is best to replicate what has been done for an existing testcase.

- For example, `git grep bytearraymini` should show all the places where the test case
`bytearraymini` has been used. Replicating the same files for a new test case
should "just work".
