#!/bin/sh

(echo 'Benchmark;Runtime change;Original runtime'; ./bench-cmp.pl "$@") | column -ts\;
