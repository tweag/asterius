#!/bin/sh -e

ahc-cabal v1-install -j$jobs --ghc-option=-j$jobs \
  Cabal
