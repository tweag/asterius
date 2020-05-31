#!/bin/sh

set -eu

sudo apt install -y \
  alex \
  happy \
  libnuma-dev
curl -L https://github.com/WebAssembly/binaryen/archive/version_93.tar.gz | tar xz -C /tmp
cd /tmp/binaryen-version_93
mkdir build
cd build
cmake \
  -DCMAKE_BUILD_TYPE=Release \
  -DCMAKE_INSTALL_PREFIX=/usr \
  -G "Unix Makefiles" \
  ..
sudo make -j2 install
