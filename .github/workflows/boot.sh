#!/bin/bash

set -euo pipefail

stack update
stack -j2 build --test --no-run-tests asterius
. .envrc
ahc-boot
