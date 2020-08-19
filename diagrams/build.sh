#!/bin/sh

set -eu

ahc-link \
  --input-hs hilbert.hs \
  --browser \
  --bundle \
  --yolo

rm -f *.hi *.o *.mjs

chown $UID:$GID *

tar -cf ../hilbert.tar \
  *.html \
  *.js \
  *.wasm
