#!/bin/sh -e

export ROOT=$(stack path --local-install-root)

stack clean \
  asterius \
  ghc-toolkit
rm -rf \
  $ROOT/share/*/asterius* \
  $ROOT/share/*/ghc-toolkit* \
  asterius/.stack-work \
  ghc-toolkit/.stack-work
