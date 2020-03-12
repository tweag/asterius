#!/bin/bash -e

shopt -s globstar

ghcid --command="stack exec ghci -- -package ghc -Wall -j -fno-code +RTS -A64m -n2m -RTS $(echo asterius/src/**/*.hs)"
