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
  http://deb.debian.org/debian/pool/main/b/binaryen/binaryen_99-2_amd64.deb
sudo dpkg -i /tmp/binaryen.deb
rm /tmp/binaryen.deb

sudo mkdir -p /opt/wasi-sdk
curl -L https://github.com/TerrorJack/wasi-sdk/releases/download/210113/wasi-sdk-12.1g41fa3294474c-linux.tar.gz | sudo tar xz -C /opt/wasi-sdk --strip-components=1
