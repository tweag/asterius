#!/bin/sh -e

ahc-cabal v1-install -j$jobs --package-db=clear --package-db=global --ghc-option=-j$jobs \
  Cabal
