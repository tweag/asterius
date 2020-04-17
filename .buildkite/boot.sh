#!/bin/bash

set -euo pipefail

sudo mount -t tmpfs tmpfs /tmp

cp --no-preserve=ownership -r $(pwd) /tmp/asterius

cd /tmp/asterius

stack --no-terminal -j8 build --test --no-run-tests asterius
. .envrc
ahc-boot

sudo tar -C / --zstd -cf /asterius/asterius.tar.zst \
  /home/asterius/.stack \
  /tmp/asterius
sudo chown $UID:$GID /asterius/asterius.tar.zst
