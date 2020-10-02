#!/bin/sh

set -eu

npm install -g \
  @cloudflare/wrangler \
  webpack@next \
  webpack-cli

sudo apt install -y \
  alex \
  c2hs \
  cpphs \
  happy

curl \
  -o /tmp/binaryen.deb \
  http://deb.debian.org/debian/pool/main/b/binaryen/binaryen_97-1_amd64.deb
sudo dpkg -i /tmp/binaryen.deb
rm /tmp/binaryen.deb

curl \
  -L \
  -o /tmp/wasi-sdk.deb \
  https://github.com/WebAssembly/wasi-sdk/releases/download/wasi-sdk-11/wasi-sdk_11.0_amd64_ubuntu20.04.deb
sudo dpkg -i /tmp/wasi-sdk.deb
rm /tmp/wasi-sdk.deb
