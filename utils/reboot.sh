#!/bin/sh -e

export CPUS=$(getconf _NPROCESSORS_ONLN 2>/dev/null)
export MAKEFLAGS=-j$CPUS

. utils/clean.sh
stack build -j$CPUS --test --no-run-tests
stack exec ahc-boot
