#!/bin/sh -e

rm -rf ~/.cabal

ahc-cabal v1-update

ahc-cabal v1-install -j --keep-going --minimize-conflict-set --prefix=$ASTERIUS_LIB_DIR --package-db=clear --package-db=global Cabal
ahc-cabal v1-install -j --keep-going --minimize-conflict-set --prefix=$ASTERIUS_LIB_DIR --package-db=clear --package-db=global \
  backprop \
  cassava-megaparsec \
  cheapskate-lucid \
  cmark \
  composition-extra \
  concurrency \
  criterion \
  Earley \
  general-games \
  generics-sop \
  genvalidity-aeson \
  graphite \
  groundhog-th \
  microlens-platform \
  persistent \
  relational-query \
  resourcet \
  rio \
  safe-money \
  selda-json \
  serialise \
  shake \
  skylighting \
  streamly \
  tasty-hspec

rm -rf ~/.cabal
