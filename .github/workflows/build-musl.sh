#!/bin/sh

set -eu

echo "http://dl-cdn.alpinelinux.org/alpine/edge/testing" >> /etc/apk/repositories
apk upgrade
apk add \
  alpine-sdk \
  autoconf \
  automake \
  binutils-gold \
  coreutils \
  findutils \
  gawk \
  grep \
  ghc \
  ncurses-dev \
  py3-sphinx \
  sed \
  util-linux \
  xz
ln -s /usr/lib/libncursesw.so.6 /usr/lib/libtinfow.so.6

mkdir -p ~/.local/bin
curl -L https://github.com/commercialhaskell/stack/releases/download/v2.3.3/stack-2.3.3-linux-x86_64-bin -o ~/.local/bin/stack
chmod +x ~/.local/bin/stack
mkdir ~/.stack
printf "system-ghc: true\nghc-build: musl\n" > ~/.stack/config.yaml
~/.local/bin/stack --resolver lts-16.13 install \
  alex \
  happy \
  hscolour

cd /tmp
git clone --recurse-submodules --branch $BRANCH https://github.com/TerrorJack/ghc.git
cd /asterius

export PATH=~/.local/bin:$PATH
mv .github/workflows/build-linux.mk /tmp/ghc/mk/build.mk
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
