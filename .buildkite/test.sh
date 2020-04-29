#!/bin/bash

set -euo pipefail

sudo mount -t tmpfs tmpfs /tmp

tar -C / --zstd -xf asterius.tar.zst

cd /tmp/asterius

stack --no-terminal test asterius:fib
stack --no-terminal test asterius:jsffi
stack --no-terminal test asterius:array
stack --no-terminal test asterius:stableptr
stack --no-terminal test asterius:rtsapi
stack --no-terminal test asterius:teletype
stack --no-terminal test asterius:bytearray
stack --no-terminal test asterius:bigint
stack --no-terminal test asterius:todomvc
stack --no-terminal test asterius:cloudflare
stack --no-terminal test asterius:exception
stack --no-terminal test asterius:regression60
stack --no-terminal test asterius:sizeof_md5context
stack --no-terminal test asterius:largenum

stack --no-terminal test asterius:fib --test-arguments="--backend=wasm-toolkit"
stack --no-terminal test asterius:jsffi --test-arguments="--backend=wasm-toolkit"
stack --no-terminal test asterius:array --test-arguments="--backend=wasm-toolkit"
stack --no-terminal test asterius:stableptr --test-arguments="--backend=wasm-toolkit"
stack --no-terminal test asterius:rtsapi --test-arguments="--backend=wasm-toolkit"
stack --no-terminal test asterius:teletype --test-arguments="--backend=wasm-toolkit"
stack --no-terminal test asterius:bytearray --test-arguments="--backend=wasm-toolkit"
stack --no-terminal test asterius:bigint --test-arguments="--backend=wasm-toolkit"
stack --no-terminal test asterius:todomvc --test-arguments="--backend=wasm-toolkit"
stack --no-terminal test asterius:cloudflare --test-arguments="--backend=wasm-toolkit"
stack --no-terminal test asterius:exception --test-arguments="--backend=wasm-toolkit"
stack --no-terminal test asterius:regression60 --test-arguments="--backend=wasm-toolkit"
stack --no-terminal test asterius:sizeof_md5context --test-arguments="--backend=wasm-toolkit"
stack --no-terminal test asterius:largenum --test-arguments="--backend=wasm-toolkit"

stack --no-terminal test asterius:bytearray --test-arguments="--yolo"
stack --no-terminal test asterius:bytearray --test-arguments="--gc-threshold=128"
stack --no-terminal test asterius:fib --test-arguments="--no-gc-sections"

stack --no-terminal test asterius:fib --test-arguments="--debug" > /dev/null
stack --no-terminal test asterius:jsffi --test-arguments="--debug" > /dev/null
stack --no-terminal test asterius:array --test-arguments="--debug" > /dev/null
stack --no-terminal test asterius:stableptr --test-arguments="--debug" > /dev/null
stack --no-terminal test asterius:rtsapi --test-arguments="--debug" > /dev/null
stack --no-terminal test asterius:teletype --test-arguments="--debug" > /dev/null
# stack --no-terminal test asterius:bytearray --test-arguments="--debug" > /dev/null
stack --no-terminal test asterius:bigint --test-arguments="--debug" > /dev/null
stack --no-terminal test asterius:exception --test-arguments="--debug" > /dev/null

stack --no-terminal test asterius:fib --test-arguments="--tail-calls"
stack --no-terminal test asterius:fib --test-arguments="--tail-calls --no-gc-sections"

stack --no-terminal test asterius:nomain
stack --no-terminal test asterius:nomain --test-arguments="--tail-calls"

stack --no-terminal test asterius:th

stack --no-terminal test asterius:primitive
