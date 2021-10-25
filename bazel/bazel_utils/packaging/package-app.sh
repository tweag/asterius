#!/usr/bin/env bash
# Copyright (c) 2021 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
# SPDX-License-Identifier: Apache-2.0

# package-app <binary> <output file> <resources...>

# On Linux and MacOS we handle dynamically linked binaries as follows:
# 1. Create a temporary directory and lib directory
# 2. Copy the binary to the temporary directory
# 3. Use platform specific tools to extract names of the
#    dynamic libraries and copy them to lib directory.
# 4. Create a wrapper or patch the binary.
# 5. Create tarball out of the temporary directory.
# 6. Clean up.

# On Windows we only handle statically linked binaries
# (Haskell binaries are linked statically on Windows) so we
# just copy the binary and the resources and create a tarball from that.

# Copy-pasted from the Bazel Bash runfiles library v2.
set -xe
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
    patchelf=$(abspath $(rlocation patchelf/bin/patchelf))
    ldd=$(abspath $(rlocation glibc-bin/bin/ldd))
    rev=$(abspath $(rlocation utillinux/bin/rev))
    ;;
  CYGWIN*|MINGW*|MSYS*)
    tar=$(abspath $(rlocation tar/usr/bin/tar.exe))
    gzip=$(abspath $(rlocation gzip/urs/bin/gzip.exe))
    mktgz=$(abspath $(rlocation bazel_asterius/bazel/bazel_utils/sh/mktgz.exe))
    ldd=$(abspath $(rlocation glibc-bin/bin/ldd.exe))
    rev=$(abspath $(rlocation utilslinux/bin/rev.exe))
    ;;
esac

set -eou pipefail

WORKDIR="$(mktemp -d)"
trap "rm -rf $WORKDIR" EXIT

SRC=$(abspath $1)
OUT=$(abspath $2)
shift 2
NAME=$(basename $SRC)
mkdir -p $WORKDIR/$NAME/dynlib
mkdir -p "$WORKDIR/$NAME/bin_$NAME"
export ORIGIN=$(dirname $(canonicalpath $SRC)) # for rpaths relative to binary

# Copy in resources, if any.
if [ $# -gt 0 ]; then
  mkdir -p $WORKDIR/$NAME/resources
  for res in $*; do
    if [[ "$res" == *.tar.gz ]]; then
      # If a resource is a tarball, e.g., because it originates from another
      # rule we extract it.
      $tar xf "$res" --strip-components=1 -C "$WORKDIR/$NAME/resources"
    else
      cp -aL "$res" "$WORKDIR/$NAME/resources"
    fi
  done
fi

# Copy the binary, patch it, and create a wrapper script if necessary.
if [ "$(uname -s)" == "Linux" ]; then
  binary="$WORKDIR/$NAME/bin_$NAME/$NAME"
  cp $SRC $binary
  chmod u+w $binary
  rpaths_binary=$($patchelf --print-rpath "$binary"|tr ':' ' ')
  function copy_deps {
    local from target needed libOK rpaths interpreter
    from=$1
    target=$2
    needed=$($patchelf --print-needed "$from")
    rpaths="$($patchelf --print-rpath "$from"|tr ':' ' ') $rpaths_binary"

    for lib in $needed; do
      if [ ! -f "$target/$lib" ]; then
        libOK=0
        for rpath in $rpaths; do
          rpath="$(eval echo $rpath)" # expand variables, e.g. $ORIGIN
          if [ -e "$rpath/$lib" ]; then
            libOK=1
            if [ "$lib" != "ld-linux-x86-64.so.2" ]; then
              # clear the old rpaths (silence stderr as it always warns
              # with "working around a Linux kernel bug".
              cp "$rpath/$lib" "$target/$lib"
              chmod u+w "$target/$lib"
              $patchelf --set-rpath '$ORIGIN' "$target/$lib" 2> /dev/null
	      copy_deps "$rpath/$lib" "$target"
	    else
	      cp -f "$rpath/$lib" "$WORKDIR/$NAME/$lib"
              chmod u+w "$WORKDIR/$NAME/$lib"
            fi
            break
          fi
        done
        if [ $libOK -ne 1 ]; then
	  libPath=$($ldd "$from" | grep "$lib" | $rev | cut -d " " -f2 | $rev | xargs)
	  echo "libPath=$libPath"
	  if [ -e "$libPath" ]; then
	    libOK=1
	    if [ "$lib" != "ld-linux-x86-64.so.2" ]; then
	    # clear the old rpaths (silence stderr as it always warns
	    # with "working around a Linux kernel bug".
	      cp "$libPath" "$target/$lib"
	      chmod u+w "$target/$lib"
              $patchelf --set-rpath '$ORIGIN' "$target/$lib" 2> /dev/null
	      copy_deps "$libPath" "$target"
	    else
	      cp -f "$libPath" "$WORKDIR/$NAME/$lib"
              chmod u+w "$WORKDIR/$NAME/$lib"
	    fi
	  fi
	fi
	if [ $libOK -ne 1 ]; then
          echo "ERROR: Dynamic library $lib for $from not found from RPATH!"
          echo "RPATH=$rpaths"
          return 1
        fi
      fi
    done
  }

  # Copy the binary's dynamic library dependencies.
  copy_deps "$binary" "$WORKDIR/$NAME/dynlib"

  # Workaround for dynamically loaded name service switch libraries
  (shopt -s nullglob
   for rpath in $rpaths_binary; do
     for nss in $rpath/libnss_dns* $rpath/libnss_files* $rpath/libresolv*; do
       cp -af "$nss" "$WORKDIR/$NAME/dynlib"
     done
   done
  )

  $patchelf --set-rpath '$ORIGIN/dynlib' "$binary"
  $patchelf --set-interpreter ld-undefined.so "$binary"

  # Link resources directory to lib, as that'll be the actual
  # binary's location.
  test -d "$WORKDIR/$NAME/resources" &&
    ln -s "../resources" "$WORKDIR/$NAME/dynlib/resources"

  # Create the wrapper script
  wrapper="$WORKDIR/$NAME/$NAME"
  cat > "$wrapper" <<EOF
#!/usr/bin/env sh
SOURCE_DIR="\$(cd \$(dirname \$(readlink -f "\$0")); pwd)"
LIB_DIR="\$SOURCE_DIR/dynlib"
if [ -z "\${LOCALE_ARCHIVE}" -a -f "/usr/lib/locale/locale-archive" ]; then
  export LOCALE_ARCHIVE="/usr/lib/locale/locale-archive"
fi
# Execute the wrapped application through the provided dynamic linker
exec \$SOURCE_DIR/ld-linux-x86-64.so.2 --library-path "\$LIB_DIR" "\$SOURCE_DIR/bin_$NAME/$NAME" "\$@"
EOF
  chmod a+x "$wrapper"
elif [[ "$(uname -s)" == "Darwin" ]]; then
  cp $SRC $WORKDIR/$NAME/$NAME
  chmod u+w $WORKDIR/$NAME/$NAME
  function copy_deps() {
    local from_original=$(canonicalpath $1)
    local from_copied=$2
    local needed="$(/usr/bin/otool -L "$from_copied" | sed -n -e '1d' -e 's/^\s*\([^ ]*\).*$/\1/p')"
    # Note that it is crucial that we resolve loader_path relative to from_original instead of from_copied
    loader_path="$(dirname $from_original)"
    local rpaths="$(/usr/bin/otool -l $from_original | sed -n '/cmd LC_RPATH/{n;n;p;}' | sed -n -e 's/^ *path \([^ ]*\).*$/\1/p' | sed -e "s|@loader_path|$loader_path|")"
    for lib in $needed; do
      local libName="$(basename $lib)"
      if [[ "$libName" == "libSystem.B.dylib" ]]; then
          /usr/bin/install_name_tool -change "$lib" "/usr/lib/$libName" "$from_copied"
      elif [ -e "/usr/lib/system/$libName" ]; then
          /usr/bin/install_name_tool -change "$lib" "/usr/lib/system/$libName" "$from_copied"
      elif [[ "$lib" == @rpath/* ]]; then
          libName="${lib#@rpath/}"
          /usr/bin/install_name_tool -change "$lib" "@rpath/$(basename $libName)" "$from_copied"
          local to="$WORKDIR/$NAME/dynlib/$(basename $libName)"
          if [[ ! -f "$to" ]]; then
              libOK=0
              for rpath in $rpaths; do
                  if [[ -e "$rpath/$libName" ]]; then
                      libOK=1
                      cp "$rpath/$libName" "$to"
                      chmod 0755 "$to"
                      /usr/bin/install_name_tool -add_rpath "@loader_path/dynlib" "$to"
                      copy_deps "$rpath/$libName" "$to"
                      break
                  fi
              done
              if [[ $libOK -ne 1 ]]; then
                  echo "ERROR: Dynamic library $lib for $from_original not found from RPATH!"
                  echo "RPATH=$rpaths"
                  return 1
              fi
          fi
      else
        /usr/bin/install_name_tool -change "$lib" "@rpath/$libName" "$from_copied"

        local to="$WORKDIR/$NAME/dynlib/$libName"
        if [ ! -f "$to" ]; then
          cp "$lib" "$to"
          chmod 0755 "$to"
          /usr/bin/install_name_tool -add_rpath "@loader_path/dynlib" "$to"

          # Traverse the library as well to find the closure
          copy_deps "$lib" "$to"
        fi
      fi
    done
  }
  # Set the dynamic library load path to the relative lib/ directory.
  /usr/bin/install_name_tool -add_rpath "@loader_path/dynlib" $WORKDIR/$NAME/$NAME

  # Copy all dynamic library dependencies referred to from our binary
  copy_deps $SRC $WORKDIR/$NAME/$NAME
else
    cp "$SRC" "$WORKDIR/$NAME/$NAME"
fi
# cd $WORKDIR && $mktgz $OUT $NAME
$mktgz $OUT -C "$WORKDIR/$NAME" .
echo "after"
ls $OUT
