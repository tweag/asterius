#!/bin/sh

set -eu

. ./.envrc
stack update
stack -j2 build --test --no-run-tests asterius
ahc-boot
