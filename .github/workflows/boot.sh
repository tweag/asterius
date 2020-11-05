#!/bin/sh

set -eu

stack update

stack -j2 build --test --no-run-tests \
  asterius \
  ghc-bin-asterius \
  ghc-pkg-asterius

. ./.envrc

ahc-boot
