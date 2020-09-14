#!/bin/bash

set -euo pipefail

pacman -S --needed --noconfirm \
  git \
  mingw-w64-x86_64-python-sphinx
git config --global core.autocrlf false
git config --global core.eol lf

pushd /c/Users/runneradmin/AppData/Roaming
mkdir -p local/bin
curl -L https://github.com/commercialhaskell/stack/releases/download/v2.3.3/stack-2.3.3-windows-x86_64-bin.exe -o local/bin/stack.exe
mkdir stack
echo "skip-msys: true" > stack/config.yaml
popd

export PATH=/c/Users/runneradmin/AppData/Roaming/local/bin:$PATH
stack update
stack --resolver lts-16.14 install \
  alex \
  happy \
  hscolour
export PATH=$(stack path --compiler-bin):$PATH

pushd /tmp
git clone --recurse-submodules --branch $BRANCH https://github.com/TerrorJack/ghc.git
popd

cp .github/workflows/build-windows.mk /tmp/ghc/mk/
pushd /tmp/ghc
./boot
./configure --enable-tarballs-autodownload
make -j$JOBS
make binary-dist
mkdir ghc-bindist
mv *.tar.* ghc-bindist/
(ls -l ghc-bindist && sha256sum -b ghc-bindist/*) > ghc-bindist/sha256.txt
popd

mv /tmp/ghc/ghc-bindist .
