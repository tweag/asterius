#!/bin/sh

set -eu

npm install -g parcel-bundler@1.12.4

sudo apt install -y \
  alex \
  c2hs \
  cpphs \
  happy \
  libnuma-dev

curl -L https://github.com/WebAssembly/binaryen/archive/version_94.tar.gz | tar xz -C /tmp
cd /tmp/binaryen-version_94
mkdir build
cd build
cmake \
  -DCMAKE_BUILD_TYPE=Release \
  -DCMAKE_INSTALL_PREFIX=/usr \
  -G "Unix Makefiles" \
  ..
sudo make -j2 install
