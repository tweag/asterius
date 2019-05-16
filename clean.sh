#!/bin/bash -e

shopt -s globstar

cd asterius/test/ghc-testsuite

rm -rf **/*.hi **/*.o **/*.wasm **/*.mjs
