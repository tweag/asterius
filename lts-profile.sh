#!/bin/sh -e

ahc-cabal v1-install -j$jobs --prefix=$ASTERIUS_LIB_DIR --package-db=clear --package-db=global --keep-going \
  Cabal
