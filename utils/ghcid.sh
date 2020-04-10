#!/bin/bash -e

shopt -s globstar

cd asterius

stack exec ghci -- \
  -package ghc \
  -Wall \
  -Wno-overflowed-literals \
  -j \
  -fno-code \
  +RTS -N -A64m -n2m -RTS \
  $(echo src-types/**/*.hs) \
  $(echo src/**/*.hs) \
  $(echo $(stack path --dist-dir)/build/autogen/**/*.hs)
