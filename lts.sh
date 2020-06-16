#!/bin/sh

set -eu

ahc-cabal v1-install --ghc-option=-j$jobs --prefix=$ASTERIUS_LIB_DIR --package-db=clear --package-db=global \
  Cabal

cp $(which node) /tmp
cp $(which false) $(which node)
ahc-cabal v1-install -j$jobs --prefix=$ASTERIUS_LIB_DIR --package-db=clear --package-db=global --keep-going \
  $(cat pkgs.txt) || true

mv /tmp/node $(which node)
ahc-cabal v1-install -j$jobs_th --prefix=$ASTERIUS_LIB_DIR --package-db=clear --package-db=global --keep-going \
  $(cat pkgs.txt) || true
