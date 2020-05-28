#!/bin/sh

set -eu

pwd
ls -a
stack update
stack -j2 build --test --no-run-tests asterius
. .envrc
ahc-boot
