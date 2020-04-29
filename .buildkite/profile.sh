#!/bin/bash

set -euo pipefail

cd .buildkite

GHCRTS="-P -hy -l-au" ahc-link --input-hs Setup.hs

mkdir reports
mv \
  *.eventlog \
  *.hp \
  *.prof \
  reports/
