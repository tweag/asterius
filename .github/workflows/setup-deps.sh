#!/bin/sh

set -eu

npm install -g parcel-bundler@1.12.4

sudo apt install -y \
  alex \
  c2hs \
  cpphs \
  happy

curl \
  --retry 8 \
  --retry-delay 1 \
  -o /tmp/binaryen.deb \
  https://snapshot.debian.org/archive/debian/20200811T024041Z/pool/main/b/binaryen/binaryen_95-1_amd64.deb
sudo dpkg -i /tmp/binaryen.deb
rm /tmp/binaryen.deb
