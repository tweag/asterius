# Building guide

## Building and using `asterius` locally

Asterius is organized as a `stack` project. The main reason is: `stack` has
builtin support for managing different sandboxed GHC installations, so it can
automatically set up the custom GHC fork described below.

In principle, building with `cabal` should also work, but this hasn't been
confirmed yet. Some additional work is needed (checking in generated `.cabal`
files, setting up a `cabal` project, etc) and PRs are welcome.

### About the custom GHC fork

Asterius is currently based on a custom GHC fork maintained
[here](https://github.com/TerrorJack/ghc). We provide prebuilt bindists for x64
Linux, so normally there's no need to build GHC, at least on a conventional
`glibc`-based distro. In case the bindists don't work for some reason (do open
an issue!), below is more information on the fork and how to build it.

The fork applies a few patches on top of an upstream release branch. For
instance, right now we're based on GHC 8.8, so our branch is `asterius-8.8`,
adding the patches on top of the upstream `ghc-8.8` branch. See the
[commits](https://github.com/TerrorJack/ghc/commits/asterius-8.8) for a list of
our patches.

We build several different variants of GHC bindists on CircleCI. See the
[`circleci-ghc-bindist`](https://github.com/tweag/asterius/tree/circleci-ghc-bindist)
branch of the `asterius` repo for the CI scripts to build GHC bindists. For now,
the scripts are still using the `make`-based build system for better
compatibility with `stack setup`. The build results are available as CircleCI
artifacts, and we include them in the
[`stack.yaml`](https://github.com/tweag/asterius/blob/master/stack.yaml) file of
`asterius`.

### System dependencies

In addition to regular GHC dependencies, these dependencies are
needed in the local environment:

* `libnuma-dev` (Required by GHC)
* `cmake`, `g++`, `git`, `python3` (Required by `binaryen`)
* `automake`, `autoconf` (Required by `ahc-boot`)
* `cabal` (at least `v3.0.0.0`)
* `node`, `npm` (at least `v12`)

### Building `asterius`

Check out the `asterius` repo, run `stack build asterius`.

Set the `MAKEFLAGS` environment variable to pass additional flags to `make` when
building `binaryen`, e.g. `MAKEFLAGS=-j8` for parallel building with 8 CPU
cores.

After the `asterius` package is built, run `stack exec ahc-boot` to perform
booting. This will compile the standard libraries to WebAssembly and populate
the `asterius` global package database. Some packages are compiled using
`ahc-cabal` in the boot process, so internet is required at least for the first
boot.

### Calling executables of `asterius`

After the booting process completes, it's possible to use `stack exec` to call
executables of `asterius`, e.g. `ahc-link` or `ahc-cabal`. Although it's
possible to use `stack install asterius` to install the executables to somewhere
in `PATH` and directly call them later, this is not recommended, since the
`asterius` executables rely on certain components in the `PATH` set up by `stack
exec`.

If [`direnv`](https://direnv.net) is enabled, then the shell session can
automatically set up the correct `PATH` when navigating into the `asterius`
project directory. Thus it's possible to directly call `ahc-boot` for booting,
`ahc-link` for compiling, etc.

For trying small examples, it's convenient to put them in the `test` directory
under the project root directory, since it's a `.gitignore` item, so they won't
be tracked by `git`.

## Building and using `asterius` with Docker

### Using the prebuilt Docker image

The recommended way of trying `asterius` is using our prebuilt Docker image on
[Docker Hub](https://hub.docker.com/r/terrorjack/asterius). The image is updated
regularly upon new `master` branch commits, and also ships ~2k prebuilt
[packages](https://github.com/tweag/asterius/issues/354) from a recent stackage
snapshot, so it's convenient to test simple examples which use common
dependencies without needing to set up a `cabal` project.

To use the image, mount the working directory containing the Haskell source code
as a Docker shared volume, then use the `ahc-link` program:

```
username@hostname:~/project$ docker run --rm -it -v $(pwd):/project -w /project terrorjack/asterius
asterius@hostname:/project$ ahc-link --input-hs main.hs
```

Check the official
[reference](https://docs.docker.com/engine/reference/commandline/run) of `docker
run` to learn more about the command given in the example above. The example
opens an interactive `bash` session for exploration, but it's also possible to
use `docker run` to invoke the Asterius compiler on local Haskell source files.
Note that [`podman`](https://podman.io) can be used instead of `docker` here.

### Building the Docker images

The prebuilt Docker image can be reproduced by building from the in-tree
`Dockerfile`s.

`base.Dockerfile` can be used for building the base image. The base image
contains an out-of-the-box installation of `asterius`, but doesn't come with the
additional stackage packages. There's very aggressive trimming logic in
`base.Dockerfile` to make the image slimmer, so in the resulting base image,
there isn't a complete `stack` project directory for `asterius`, and it's not
possible to modify the Haskell logic of `asterius` and partially rebuild/reboot
it given a base image.

`stackage.Dockerfile` can be used for building the image containing additional
stackage packages upon the base image. Modify `lts.sh` for adding/removing
packages to be built into the final image, and
`ghc-toolkit/boot-libs/cabal.config` for modifying the package version
constraints. All the stackage packages are installed into the `asterius` global
package database, so they can be directly used by `ahc-link`, but this shouldn't
affect `ahc-cabal` for installing other versions of those packages elsewhere.

### The image for VSCode remote containers

`dev.Dockerfile` is used to build `terrorjack/asterius:dev`, which is the image
for VSCode remote containers.
