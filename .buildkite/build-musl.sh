#!/bin/sh

set -euo pipefail

mount -t tmpfs tmpfs /tmp

echo "http://dl-cdn.alpinelinux.org/alpine/edge/testing" >> /etc/apk/repositories
apk update --no-progress
apk upgrade --no-progress
apk add --no-progress \
  alpine-sdk \
  autoconf \
  automake \
  binutils-gold \
  bzip2 \
  coreutils \
  curl \
  file \
  findutils \
  g++ \
  gawk \
  ghc \
  git \
  gzip \
  libtool \
  musl-dev \
  ncurses-dev \
  numactl-dev \
  openssh \
  patch \
  py3-sphinx \
  sed \
  tar \
  xz \
  zlib-dev
mkdir -p ~/.local/bin
curl -L https://github.com/commercialhaskell/stack/releases/download/v2.3.0.1/stack-2.3.0.1-linux-x86_64-static.tar.gz | tar xz --wildcards --strip-components=1 -C ~/.local/bin '*/stack'
~/.local/bin/stack --system-ghc --resolver nightly-2020-04-18 --no-terminal install \
  alex \
  happy \
  hscolour

cd /tmp
git clone --recurse-submodules --branch $BRANCH --depth=1 https://github.com/TerrorJack/ghc.git
cd /asterius

export PATH=~/.local/bin:$PATH
mv .buildkite/build.mk /tmp/ghc/mk/
cd /tmp/ghc
./boot
./configure --disable-ld-override
make
make binary-dist
mkdir ghc-bindist
mv *.tar.* ghc-bindist/
(ls -l ghc-bindist && sha256sum -b ghc-bindist/*) > ghc-bindist/sha256.txt
cd /asterius

mv /tmp/ghc/ghc-bindist .
chown -c -h -R $UID:$GID ghc-bindist
