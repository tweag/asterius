#!/bin/sh

set -eu

ahc-cabal v1-install --ghc-option=-j$jobs --prefix=$ASTERIUS_LIB_DIR --package-db=clear --package-db=global \
  Cabal

export ASTERIUS_TH_IGNORE=1
ahc-cabal v1-install -j$jobs --prefix=$ASTERIUS_LIB_DIR --package-db=clear --package-db=global --keep-going \
  $(cat pkgs.txt) || true

unset ASTERIUS_TH_IGNORE
ahc-cabal v1-install -j$jobs_th --prefix=$ASTERIUS_LIB_DIR --package-db=clear --package-db=global --keep-going \
  $(cat pkgs.txt) || true
