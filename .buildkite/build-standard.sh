#!/bin/bash

set -euo pipefail

mount -t tmpfs tmpfs /tmp

echo 'deb [check-valid-until=no] http://snapshot.debian.org/archive/debian/20200417T205512Z stretch main contrib non-free' > /etc/apt/sources.list
echo 'deb [check-valid-until=no] http://snapshot.debian.org/archive/debian-security/20200417T205512Z stretch/updates main contrib non-free' >> /etc/apt/sources.list
echo 'deb [check-valid-until=no] http://snapshot.debian.org/archive/debian/20200417T205512Z stretch-updates main contrib non-free' >> /etc/apt/sources.list
apt update
apt full-upgrade -y
apt install -y \
  automake \
  build-essential \
  curl \
  gawk \
  git \
  libffi-dev \
  libgmp-dev \
  libncurses-dev \
  libnuma-dev \
  libtool-bin \
  pkg-config \
  python3-sphinx \
  xz-utils \
  zlib1g-dev
mkdir -p ~/.local/bin
curl -L https://github.com/commercialhaskell/stack/releases/download/v2.3.0.1/stack-2.3.0.1-linux-x86_64.tar.gz | tar xz --wildcards --strip-components=1 -C ~/.local/bin '*/stack'
~/.local/bin/stack --resolver nightly-2020-04-18 --no-terminal install \
  alex \
  happy \
  hscolour

pushd /tmp
git clone --recurse-submodules --branch $BRANCH --depth=1 https://github.com/TerrorJack/ghc.git
popd

export PATH=~/.local/bin:$(~/.local/bin/stack path --compiler-bin):$PATH
mv .buildkite/build.mk /tmp/ghc/mk/
pushd /tmp/ghc
./boot
./configure
make
make binary-dist
mkdir ghc-bindist
mv *.tar.* ghc-bindist/
(ls -l ghc-bindist && sha256sum -b ghc-bindist/*) > ghc-bindist/sha256.txt
popd

mv /tmp/ghc/ghc-bindist .
chown -c -h -R $UID:$GID ghc-bindist
