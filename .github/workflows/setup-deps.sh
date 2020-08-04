#!/bin/sh

set -eu

npm install -g parcel-bundler@1.12.4

sudo apt install -y \
  alex \
  c2hs \
  cpphs \
  happy

curl https://snapshot.debian.org/archive/debian/20200804T084936Z/pool/main/b/binaryen/binaryen_95-1_amd64.deb -o /tmp/binaryen.deb
sudo dpkg -i /tmp/binaryen.deb
rm /tmp/binaryen.deb
