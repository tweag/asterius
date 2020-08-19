#!/bin/sh

set -eu

npm install

ahc-link \
  --input-hs todomvc.hs \
  --browser \
  --bundle \
  --yolo

rm -f *.hi *.o *.mjs

chown $UID:$GID *

tar -cf ../todomvc.tar \
  *.html \
  *.js \
  *.wasm
