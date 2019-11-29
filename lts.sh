#!/bin/sh -e

rm -rf ~/.cabal

ahc-cabal v1-update

ahc-cabal v1-install -j --keep-going --minimize-conflict-set --prefix=$ASTERIUS_LIB_DIR --package-db=clear --package-db=global \
  cassava-megaparsec \
  cmark \
  criterion \
  generics-sop \
  genvalidity-aeson \
  graphite \
  groundhog-th \
  lucid \
  microlens-platform \
  persistent \
  relational-query \
  resourcet \
  rio \
  selda-json \
  serialise \
  shake \
  skylighting \
  tasty-hspec

rm -rf ~/.cabal
