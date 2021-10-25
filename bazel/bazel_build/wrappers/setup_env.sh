#!/usr/bin/env bash

export asterius_bindir="$(cd "$(dirname "$0")"; pwd)"
export bundle_root="$(dirname $(dirname $asterius_bindir))"
export asterius_datadir="$bundle_root/asterius-0.0.1_data" # TODO update version automatically.
# export WASI_SDK_PATH="$bundle_root/resources/wasilibc/wasi-sdk-12.0/"
export WASI_SDK_PATH="$bundle_root"
