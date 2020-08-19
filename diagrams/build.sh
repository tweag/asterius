#!/bin/sh

set -eu

ahc-link \
  --input-hs hilbert.hs \
  --browser \
  --bundle \
  --yolo

rm -f *.hi *.o *.mjs

chown $UID:$GID *

tar -cf ../diagrams.tar \
  *.html \
  *.js \
  *.wasm
