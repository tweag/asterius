#!/bin/bash

set -euo pipefail

sudo mount -t tmpfs tmpfs /tmp

tar -C / --zstd -xf asterius.tar.zst

cd /tmp/asterius

GHCRTS=-N8 stack --no-terminal test asterius:ghc-testsuite --test-arguments="-j8 --timeout=300s" || true

sudo mv asterius/test-report.csv /asterius
sudo chown $UID:$GID /asterius/test-report.csv
