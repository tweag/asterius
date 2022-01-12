# Building guide

## Building and using `asterius` locally

Asterius is organized as a `stack` project at the moment. The reason is mainly
historical: `stack` has builtin support for managing different sandboxed GHC
installations, and we used to require a custom GHC fork to build, so using
`stack` has been more convenient.

In principle, building with `cabal` should also work, but this hasn't been
tested on CI yet. Some additional work is needed (checking in generated `.cabal`
files, setting up a `cabal` project, etc) and PRs are welcome.

### System dependencies

In addition to regular GHC dependencies, these dependencies are
needed in the local environment:

* `git`
* `binaryen` (at least `version_98`)
* `automake`, `autoconf` (required by `ahc-boot`)
* `cabal` (at least `v3.0.0.0`)
* `node`, `npm` (at least `v12`)
* `python3`
* `stack`
* `wasi-sdk` (the `WASI_SDK_PREFIX` environment variable must point to the
  installation)

### Preparing the source tree

After checking out, one needs to run a script to generate the in-tree private
GHC API packages required by Asterius.

```sh
$ mkdir lib
$ pushd lib
$ ../utils/make-packages.py
$ rm -rf ghc
$ popd
```

The `make-packages.py` script will checkout our custom GHC
[fork](https://github.com/TerrorJack/ghc), run `hadrian` to generate some
autogen files, and generate several Haskell packages in `lib`. A run takes ~5min
on CI. This script only needs to be run once. After that, Asterius can be built
using vanilla GHC.

If it's inconvenient to run `make-packages.py`, it's also possible to download
the generated packages from the CI artifacts. Check the CI log of a recent
commit, and one of the artifacts is named `lib`. Download and unzip it in the
project root directory.

### Building `asterius`

After checking out and running `make-packages.py`, simply run `stack build
asterius` to build it.

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
