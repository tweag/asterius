#!/bin/sh

set -eu

npm install -g parcel-bundler@1.12.4

sudo apt install -y \
  alex \
  c2hs \
  cpphs \
  happy

curl \
  -o /tmp/binaryen.deb \
  http://deb.debian.org/debian/pool/main/b/binaryen/binaryen_96-1_amd64.deb
sudo dpkg -i /tmp/binaryen.deb
rm /tmp/binaryen.deb
