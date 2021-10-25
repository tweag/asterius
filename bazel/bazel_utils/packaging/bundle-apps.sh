#!/usr/bin/env bash
# Copyright (c) 2021 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
# SPDX-License-Identifier: Apache-2.0

# Copy-pasted from the Bazel Bash runfiles library v2.
set -uo pipefail; f=bazel_tools/tools/bash/runfiles/runfiles.bash
source "${RUNFILES_DIR:-/dev/null}/$f" 2>/dev/null || \
  source "$(grep -sm1 "^$f " "${RUNFILES_MANIFEST_FILE:-/dev/null}" | cut -f2- -d' ')" 2>/dev/null || \
  source "$0.runfiles/$f" 2>/dev/null || \
  source "$(grep -sm1 "^$f " "$0.runfiles_manifest" | cut -f2- -d' ')" 2>/dev/null || \
  source "$(grep -sm1 "^$f " "$0.exe.runfiles_manifest" | cut -f2- -d' ')" 2>/dev/null || \
  { echo>&2 "ERROR: cannot find $f"; exit 1; }; f=; set -e
# --- end runfiles.bash initialization v2 ---

# Make sure that runfiles and tools are still found after we change directory.
case "$(uname -s)" in
  Darwin)
    abspath() { python -c 'import os.path, sys; sys.stdout.write(os.path.abspath(sys.argv[1]))' "$@"; }
    canonicalpath() { python -c 'import os.path, sys; sys.stdout.write(os.path.realpath(sys.argv[1]))' "$@"; }
    ;;
  *)
    abspath() { realpath -s "$@"; }
    canonicalpath() { readlink -f "$@"; }
    ;;
esac

if [[ -n ${RUNFILES_DIR:-} ]]; then
  export RUNFILES_DIR=$(abspath $RUNFILES_DIR)
fi
if [[ -n ${RUNFILES_MANIFEST_FILE:-} ]]; then
  export RUNFILES_DIR=$(abspath $RUNFILES_MANIFEST_FILE)
fi

case "$(uname -s)" in
  Darwin|Linux)
    # find tar
    tar=$(abspath $(rlocation tar/bin/tar))
    gzip=$(abspath $(rlocation gzip/bin/gzip))
    mktgz=$(abspath $(rlocation bazel_asterius/bazel/bazel_utils/sh/mktgz))
    ;;
  CYGWIN*|MINGW*|MSYS*)
    tar=$(abspath $(rlocation tar/usr/bin/tar.exe))
    gzip=$(abspath $(rlocation gzip/urs/bin/gzip.exe))
    mktgz=$(abspath $(rlocation bazel_asterius/bazel/bazel_utils/sh/mktgz.exe))
    ;;
esac

set -eou pipefail

set -x
WORKDIR="$(mktemp -d)"
trap "rm -rf $WORKDIR" EXIT


OUT=$(abspath $1)
shift 1

# Copy in resources, if any.
if [ $# -gt 0 ]; then
  for res in $*; do
    if [[ "$res" == *.tar.gz ]]; then
      # If a resource is a tarball, e.g., because it originates from another
      # rule we extract it.
      # $tar xf "$res" --strip-components=3 -C "$WORKDIR"
      $tar xf "$res" -C "$WORKDIR"
    else
      echo "res = $res"
      cp -aL "$res" "$WORKDIR"
    fi
  done
fi

# Rename BUILD file.
if [ -f "$WORKDIR/asterius_bundle.BUILD.bazel" ]; then
  mv "$WORKDIR/asterius_bundle.BUILD.bazel" "$WORKDIR/BUILD.bazel"
fi

# build a wasi_sdk_path folder with the right structure,
# because we now use the wrapper scripts and not the binaries directly.

# mkdir -p "$WORKDIR/wasi_sdk_path/bin"
# ln -s "../../ressources/wasilibc/wasi-sdk-12.0/bin/git-clang-format" "$WORKDIR/wasi_sdk_path/bin/git-clang-format"
# ln -s "../../ar" "$WORKDIR/wasi_sdk_path/bin/ar"
# ln -s "../../clang" "$WORKDIR/wasi_sdk_path/bin/clang"
# ln -s "../../clang-11" "$WORKDIR/wasi_sdk_path/bin/clang-11"
# ln -s "../../clang-cl" "$WORKDIR/wasi_sdk_path/bin/clang-cl"
# ln -s "../../clang-format" "$WORKDIR/wasi_sdk_path/bin/clang-format"
# ln -s "../../ld64.lld" "$WORKDIR/wasi_sdk_path/bin/ld64.lld"
# ln -s "../../lld-link" "$WORKDIR/wasi_sdk_path/bin/lld-link"
# ln -s "../../llvn-cxxfilt" "$WORKDIR/wasi_sdk_path/bin/llvm-cxxfilt"
# ln -s "../../llvn-nm" "$WORKDIR/wasi_sdk_path/bin/llvm-nm"
# ln -s "../../llvn-objdump" "$WORKDIR/wasi_sdk_path/bin/llvm-objdump"
# ln -s "../../llvn-size" "$WORKDIR/wasi_sdk_path/bin/llvm-size"
# ln -s "../../llvn-strip" "$WORKDIR/wasi_sdk_path/bin/llvm-strip"
# ln -s "../../objcopy" "$WORKDIR/wasi_sdk_path/bin/objcopy"
# ln -s "../../strings" "$WORKDIR/wasi_sdk_path/bin/strings"
# ln -s "../../wasm-ld" "$WORKDIR/wasi_sdk_path/bin/wasm-ld"
# ln -s "../../c++filt" "$WORKDIR/wasi_sdk_path/bin/c++filt"
# ln -s "../../clang++" "$WORKDIR/wasi_sdk_path/bin/clang++"
# ln -s "../../clang-apply-replacements" "$WORKDIR/wasi_sdk_path/bin/clang-apply-replacements"
# ln -s "../../clang-cpp" "$WORKDIR/wasi_sdk_path/bin/clang-cpp"
# ln -s "../../clang-tidy" "$WORKDIR/wasi_sdk_path/bin/clang-tidy"
# ln -s "../../ld.lld" "$WORKDIR/wasi_sdk_path/bin/ld.lld"
# ln -s "../../lld" "$WORKDIR/wasi_sdk_path/bin/lld"
# ln -s "../../llvm-ar" "$WORKDIR/wasi_sdk_path/bin/llvm-ar"
# ln -s "../../llvm-dwarfdump" "$WORKDIR/wasi_sdk_path/bin/llvm-dwarfdump"
# ln -s "../../llvm-objcopy" "$WORKDIR/wasi_sdk_path/bin/llvm-objcopy"
# ln -s "../../llvm-ranlib" "$WORKDIR/wasi_sdk_path/bin/llvm-ranlib"
# ln -s "../../llvm-strings" "$WORKDIR/wasi_sdk_path/bin/llvm-strings"
# ln -s "../../nm" "$WORKDIR/wasi_sdk_path/bin/nm"
# ln -s "../../objdump" "$WORKDIR/wasi_sdk_path/bin/objdump"
# ln -s "../../size" "$WORKDIR/wasi_sdk_path/bin/size"
# ln -s "../../strip" "$WORKDIR/wasi_sdk_path/bin/strip"

# ln -s "../resources/wasilibc/wasi-sdk-12.0/lib" "$WORKDIR/wasi_sdk_path/lib"
# ln -s "resources/wasilibc/wasi-sdk-12.0/lib" "$WORKDIR/lib"
# ln -s "../resources/wasilibc/wasi-sdk-12.0/share" "$WORKDIR/wasi_sdk_path/share"

if [ -f "$WORKDIR/wasi_folder/resources/wasilibc/wasi-sdk-12.1g41fa3294474c/lib" ]; then
  ln -s "wasi_folder/resources/wasilibc/wasi-sdk-12.1g41fa3294474c/lib" "$WORKDIR/lib"
  # ln -s "wasi_folder/resources/wasilibc/wasi-sdk-12.1g41fa3294474c/lib" "$WORKDIR/bin/lib"
fi

# Some of the binaries may need to be in the same folder to work together.
# Since all of them need to be called throught the wrapper scripts, 
# next to each binary, we add symlinks to the wrapper script of the other ones.

for target_dir_abs in $WORKDIR/bin_*; do
    target_dir=$(basename $target_dir_abs)
    echo "target_dir=$target_dir"
    target=${target_dir:4}
    echo "target=$target"
    for src_dir_abs in $WORKDIR/bin_*; do
	src_dir=$(basename $src_dir_abs)
	echo "src_dir=$src_dir"
	src=${src_dir:4}
	echo "src=$src"
	if [ $target != $src ]; then
	    ln -s "../$target" "$src_dir_abs/$target"
        fi
    done
done

# $mktgz $OUT -C "$WORKDIR" .
$mktgz $OUT -C "$WORKDIR" .
