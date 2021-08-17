#!/usr/bin/env bash

set -euo pipefail

utils/clean.sh
stack build --test --no-run-tests
. .envrc
ahc-boot
