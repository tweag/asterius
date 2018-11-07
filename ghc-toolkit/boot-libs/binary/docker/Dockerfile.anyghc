# Dockerfile to load a haskell environment for running binary's test suite.
#
# Building the Dockerfile creates an image which has the haskell environment
# with ghc and cabal setup and ready to use.
#
# Use a docker volume to cache built dependencies. It will greatly speed up
# running the tests repeatedly.
#
# Create a volume:
#
#   docker volume create cabal-store-cache
#
# How to build:
#
#   docker build \
#     -f docker/Dockerfile.ghc843 \
#     -t haskell/binary \
#     --build-arg ghcver=8.4.2 \
#     .
#
# How to run (caching the cabal store directory), default is 'cabal new-test':
#
#   docker run -it haskell/binary -v cabal-store-cache:/root/.cabal/store
#
# Run 'cabal new-bench' or any other command (bash, to get into the machine):
#
#   docker run -it haskell/binary -v cabal-store-cache:/root/.cabal/store \
#     cabal new-bench
#
# Hacks to build binary:
#
#   1) Copy all files from the host machine.
#
#   2) Rename binary to binary-next. This is an unfortunate consequence of
#      binary being used by its test and benchmark dependencies.
#      Not renaming binary will make cabal confused and it'll fail to build.
#
#      Cabal can be made to build properly by carefully installing the test
#      and benchmark dependencies manually, like it's done in .travis.yml.
#      Unfortunately that setup is very fragile since changing the
#      dependencies in binary.cabal also requires updating .travis.yml.
#      Thus .travis.yml gets out of sync when we forget.
#      This method also doesn't work with the nix-style commands which
#      themselves take care of installing dependencies.
#      The simples workaround I've found, and the only thing that works
#      with nix-style commands, is to simply rename the package
#
#   3) Do 'cabal sdist' to get only the files for source distribution.
#
#   4) Unpack the .tar.gz file from (3) and copy generics-bench.cache.gz
#      to the same dir.
#
#   5) The setup is complete. You may run cabal new-test,
#      or any other command.
#

FROM debian:stable

# setup locale.
# not setting a locale will make some apps fail when outputting utf8.
RUN apt-get update && \
    apt-get install -y locales && \
    locale-gen C.UTF-8 && \
    /usr/sbin/update-locale LANG=C.UTF-8 && \
    apt-get remove -y locales

ENV LANG C.UTF-8

# key used by haskell repo
RUN apt-get update && apt-get install -y gnupg dirmngr
RUN apt-key adv --keyserver keyserver.ubuntu.com  --recv-keys BA3CBA3FFE22B574

# add haskell repo for debian
RUN echo "deb http://downloads.haskell.org/debian stretch main" > /etc/apt/sources.list.d/haskell.list

ARG ghcver=8.4.3
ARG cabalinstallver=2.4

RUN apt update && apt install -y cabal-install-$cabalinstallver
RUN apt update && apt install -y ghc-$ghcver
RUN apt update && apt-get install -y zlib1g-dev

ENV PATH=/opt/ghc/bin:$PATH

RUN cabal new-update

COPY . /workdir/copy

WORKDIR /workdir/copy
RUN sed -i.bak -e 's/name:\s*binary/name: binary-next/' binary.cabal
RUN mv binary.cabal binary-next.cabal
RUN cabal new-sdist

WORKDIR /workdir/builddir
RUN tar xf /workdir/copy/dist-newstyle/sdist/*.tar.gz -C /workdir/builddir
RUN mv /workdir/builddir/binary-* /workdir/builddir/binary-next
# generics-bench.cache.gz is not part of the binary distribution,
# it's too large. It only lives in the git repo. Copy it manually.
RUN mv /workdir/copy/generics-bench.cache.gz /workdir/builddir/binary-next
WORKDIR /workdir/builddir/binary-next

CMD cabal new-test
