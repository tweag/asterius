#!/bin/sh -e

ahc-cabal v1-install --ipid="\$pkg-\$version" -j$jobs --ghc-option=-j$jobs \
  Cabal
