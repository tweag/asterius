#!/bin/sh -e

export CPUS=$(getconf _NPROCESSORS_ONLN 2>/dev/null)
export ASTERIUS_BUILD_OPTIONS=-j$CPUS
export MAKEFLAGS=-j$CPUS

stack build --test --no-run-tests \
  asterius \
  ghc-toolkit \
  wabt \
  wasm-toolkit

stack exec ahc-boot

direnv allow .envrc
