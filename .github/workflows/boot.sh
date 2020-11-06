#!/bin/sh

set -eu

stack update

stack -j2 build --test --no-run-tests \
  asterius

. ./.envrc

ahc-boot
