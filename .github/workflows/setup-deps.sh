#!/bin/sh

set -eu

npm install -g \
  @cloudflare/wrangler \
  webpack \
  webpack-cli

sudo apt install -y \
  alex \
  c2hs \
  cpphs \
  happy

curl \
  -o /tmp/binaryen.deb \
  http://deb.debian.org/debian/pool/main/b/binaryen/binaryen_98-1_amd64.deb
sudo dpkg -i /tmp/binaryen.deb
rm /tmp/binaryen.deb

sudo mkdir -p /opt/wasi-sdk
curl -L https://github.com/TerrorJack/wasi-sdk/releases/download/201202/wasi-sdk-11.9gb368b8b12ee6-linux.tar.gz | sudo tar xz -C /opt/wasi-sdk --strip-components=1
