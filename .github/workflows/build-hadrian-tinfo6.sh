#!/bin/bash

set -euo pipefail

export BRANCH=asterius-8.8
export DEBIAN_FRONTEND=noninteractive
export JOBS=2
export LANG=C.UTF-8
export LC_ALL=$LANG
export LC_CTYPE=$LANG
export XZ_OPT=-9eT2

echo 'deb [check-valid-until=no] http://snapshot.debian.org/archive/debian/20200910T083916Z sid main contrib non-free' > /etc/apt/sources.list
apt update
apt full-upgrade -y
apt install -y \
  autoconf \
  build-essential \
  curl \
  gawk \
  git \
  libgmp-dev \
  libncurses-dev \
  python3-minimal

mkdir -p ~/.local/bin
curl -L https://github.com/commercialhaskell/stack/releases/download/v2.3.3/stack-2.3.3-linux-x86_64-bin -o ~/.local/bin/stack
chmod +x ~/.local/bin/stack
~/.local/bin/stack --resolver lts-16.13 install \
  alex \
  happy
export PATH=~/.local/bin:$(~/.local/bin/stack path --compiler-bin):$PATH

pushd $(mktemp -d)

git clone --recurse-submodules --branch $BRANCH https://github.com/TerrorJack/ghc.git .
cp /asterius/.github/workflows/UserSettings.hs hadrian/UserSettings.hs
./boot --hadrian
./configure
hadrian/build.stack.sh -j$JOBS binary-dist
cp _build/bindist/*.tar.xz /asterius

popd
