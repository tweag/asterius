# Hacking guide

## Using VSCode remote containers

We recommend using [VSCode Remote
Containers](https://code.visualstudio.com/docs/remote/containers) to reproduce
the very same dev environment used by our core team members. The steps to set up
the dev environment are:

* Do a local clone of the asterius repo
* Install VSCode and its remote
  [extension](https://aka.ms/vscode-remote/download/extension)
* Install Docker, and make sure the `docker` command works with the current user
* `docker pull terrorjack/asterius:dev`
* Open the asterius repo with remote containers

Opening the repo with remote containers for the first time will take some time,
since it runs the build script to build `asterius` and perform booting. Later
re-opening will be near instant, since it reuses the previous container.

## Using `direnv`

If `direnv` is enabled, the `PATH` of the current shell session will be extended
to include the locations of asterius executables. This means it's possible to
run `ahc-link ..` instead of `stack exec ahc-link -- ..`.

## Hacking with `ghcid`

A known-to-work workflow of hacking asterius is using `ghcid`. We also include
an example `.ghcid` file, so running `ghcid` at the project root directory shall
work out of the box.

Some notes regarding the usage of `ghcid`:

* Multiple lib targets can be loaded at once, but only one main target
  (exe/test) can be loaded. When hacking a specific exe/test, modify the local
  `utils/ghcid.sh` script first. Before commiting changes in the Haskell
  codebase, it would be nice to run `stack build --test --no-run-tests` to make
  sure all executables are not broken by lib changes.

## To boot or not to boot

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

When rebooting, run `utils/reboot.sh` in the project root directory, so that we
can ensure the booting is used with the up-to-date version of `asterius` and the
boot lib sources.

The `ahc-boot` process is configurable via these environment variables:

* `ASTERIUS_CONFIGURE_OPTIONS`
* `ASTERIUS_BUILD_OPTIONS`
* `ASTERIUS_INSTALL_OPTIONS`

## Doing profiled builds

We have preliminary support for doing profiled builds to investigate performance
issues. Use `stack-profile.yaml` to overwrite `stack.yaml`, and then perform the
rebooting process. This will be quite slow due to the nature of profiled builds;
all libraries will be rebuilt with the profiled flavor.

Once the profiled build is complete, it's possible to use RTS flags to obtain
profile data when compiling Haskell sources. Use `GHCRTS=..` instead of `+RTS
..`, since we're often interested in the profile data of all asterius
executables, and the `GHCRTS` environment variable can propagate to all
processes.

See the relevant
[section](https://downloads.haskell.org/~ghc/8.8.3/docs/html/users_guide/profiling.html)
in the GHC user guide for more information on profiling Haskell apps. There are
also some third party applications useful for analyzing the profiling data, e.g.
[`eventlog2html`](https://github.com/mpickering/eventlog2html),
[`ghc-prof-flamegraph`](https://github.com/fpco/ghc-prof-flamegraph).

Fow now, a major problem with the profiled build is: it seems to emit
dysfunctional code which doesn't work. Consequently, this affects the TH runner,
so any dependencies relying on TH isn't supported by the profiled build.

## Adding a test case

To add a test case, it is best to replicate what has been done for an existing testcase.

- For example, `git grep bytearraymini` should show all the places where the test case
`bytearraymini` has been used. Replicating the same files for a new test case
should "just work".

## Debugging `circleCI`

All instructions documented here depend on having the `circleci` command line
tool installed.

##### Validating config

To validate the circleCI config, use:

```
circleci config validate
```

##### Run CircleCI job locally

To run a job with `circleCI` locally for debugging:

1. Install docker
2. Get the docker daemon running.

Run:

```
$ circleci local execute --job  <job-name>
```

For example:

```
$ circleci local execute --job asterius-test
```
