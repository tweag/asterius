#!/bin/sh -e

stack build --fast --file-watch --ghc-options="-H512m -j" asterius:lib
