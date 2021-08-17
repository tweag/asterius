#!/usr/bin/env bash

set -euo pipefail

export CPUS=$(getconf _NPROCESSORS_ONLN 2>/dev/null)
export MAKEFLAGS=-j$CPUS

stack update
stack build --test --no-run-tests \
  asterius \
  ghc-toolkit \
  wasm-toolkit

. .envrc
ahc-boot

direnv allow .envrc
