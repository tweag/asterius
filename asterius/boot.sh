#!/usr/bin/env bash

set -euox pipefail

AHC_TMPDIR=$(mktemp -d)
export AHC_TMPDIR

rm -rf "$AHC_LIBDIR"
mkdir -p "$AHC_LIBDIR"

cp --no-preserve=mode,ownership -r \
  "$GHC_ASTERIUS"/ghc-asterius/autogen \
  "$AHC_LIBDIR"/include
cp --no-preserve=mode,ownership \
  "$GHC_ASTERIUS_BOOT"/llvm-passes \
  "$GHC_ASTERIUS_BOOT"/llvm-targets \
  "$GHC_ASTERIUS_BOOT"/platformConstants \
  "$GHC_ASTERIUS_BOOT"/settings \
  "$GHC_ASTERIUS_BOOT"/template-hsc.h \
  "$AHC_LIBDIR"

mkdir "$AHC_LIBDIR"/package.conf.d
cp "$AHC_BOOT_SRCDIR"/rts/rts.conf "$AHC_LIBDIR"/package.conf.d
ahc-pkg --global recache

mkdir "$AHC_LIBDIR"/rts
find "$GHC_ASTERIUS_BOOT"/rts -name '*.cmm' -exec sh -c 'ahc -c -O2 -dcmm-lint -I"$AHC_LIBDIR"/include -this-unit-id rts -o "$AHC_TMPDIR"/$(basename "$0" .cmm).o "$0"' {} \;
ar qDS "$AHC_LIBDIR"/rts/libHSrts.a "$AHC_TMPDIR"/*.o

pushd "$AHC_BOOT_SRCDIR"

ASTERIUS_CONFIGURE_OPTIONS="--disable-shared --disable-profiling --disable-debug-info --disable-library-for-ghci --disable-split-objs --disable-split-sections --disable-library-stripping --enable-relocatable -O2 --prefix=$AHC_LIBDIR --global --ipid=\$pkg --with-ghc=ahc --with-ghc-pkg=ahc-pkg --hsc2hs-option=--cross-compile --ghc-option=-v1 --ghc-option=-dsuppress-ticks"

pushd "$GHC_ASTERIUS_BOOT"/ghc-prim
Setup-ghc-prim configure --builddir="$AHC_TMPDIR"/dist/ghc-prim $ASTERIUS_CONFIGURE_OPTIONS
Setup-ghc-prim build -j4 --builddir="$AHC_TMPDIR"/dist/ghc-prim
Setup-ghc-prim install --builddir="$AHC_TMPDIR"/dist/ghc-prim
popd

pushd integer-simple
ahc-cabal act-as-setup --build-type=Simple -- configure --builddir="$AHC_TMPDIR"/dist/integer-simple $ASTERIUS_CONFIGURE_OPTIONS
ahc-cabal act-as-setup --build-type=Simple -- build -j4 --builddir="$AHC_TMPDIR"/dist/integer-simple
ahc-cabal act-as-setup --build-type=Simple -- install --builddir="$AHC_TMPDIR"/dist/integer-simple
popd

pushd base
autoreconf -i
ahc-cabal act-as-setup --build-type=Configure -- configure --builddir="$AHC_TMPDIR"/dist/base -finteger-simple $ASTERIUS_CONFIGURE_OPTIONS
ahc-cabal act-as-setup --build-type=Configure -- build -j4 --builddir="$AHC_TMPDIR"/dist/base
ahc-cabal act-as-setup --build-type=Configure -- install --builddir="$AHC_TMPDIR"/dist/base
popd

pushd ghc-heap
ahc-cabal act-as-setup --build-type=Simple -- configure --builddir="$AHC_TMPDIR"/dist/ghc-heap $ASTERIUS_CONFIGURE_OPTIONS
ahc-cabal act-as-setup --build-type=Simple -- build -j4 --builddir="$AHC_TMPDIR"/dist/ghc-heap
ahc-cabal act-as-setup --build-type=Simple -- install --builddir="$AHC_TMPDIR"/dist/ghc-heap
popd

pushd ghc-boot-th
ahc-cabal act-as-setup --build-type=Simple -- configure --builddir="$AHC_TMPDIR"/dist/ghc-boot-th $ASTERIUS_CONFIGURE_OPTIONS
ahc-cabal act-as-setup --build-type=Simple -- build -j4 --builddir="$AHC_TMPDIR"/dist/ghc-boot-th
ahc-cabal act-as-setup --build-type=Simple -- install --builddir="$AHC_TMPDIR"/dist/ghc-boot-th
popd

ahc-cabal v1-update || true

ahc-cabal v1-install --only-dependencies $ASTERIUS_CONFIGURE_OPTIONS \
  unix

pushd "$(mktemp -d)"
ahc-cabal get unix-2.7.2.2
cd unix-2.7.2.2
ahc-cabal act-as-setup --build-type=Configure -- configure --builddir="$AHC_TMPDIR"/dist/unix --ghc-option=-this-unit-id=unix $ASTERIUS_CONFIGURE_OPTIONS
ahc-cabal act-as-setup --build-type=Configure -- build -j4 --builddir="$AHC_TMPDIR"/dist/unix
ahc-cabal act-as-setup --build-type=Configure -- install --builddir="$AHC_TMPDIR"/dist/unix
popd

ahc-cabal v1-install $ASTERIUS_CONFIGURE_OPTIONS \
  binary \
  directory \
  mtl \
  pretty

pushd ghc-boot
ahc-cabal act-as-setup --build-type=Simple -- configure --builddir="$AHC_TMPDIR"/dist/ghc-boot $ASTERIUS_CONFIGURE_OPTIONS
ahc-cabal act-as-setup --build-type=Simple -- build -j4 --builddir="$AHC_TMPDIR"/dist/ghc-boot
ahc-cabal act-as-setup --build-type=Simple -- install --builddir="$AHC_TMPDIR"/dist/ghc-boot
popd

pushd template-haskell
ahc-cabal act-as-setup --build-type=Simple -- configure --builddir="$AHC_TMPDIR"/dist/template-haskell $ASTERIUS_CONFIGURE_OPTIONS
ahc-cabal act-as-setup --build-type=Simple -- build -j4 --builddir="$AHC_TMPDIR"/dist/template-haskell
ahc-cabal act-as-setup --build-type=Simple -- install --builddir="$AHC_TMPDIR"/dist/template-haskell
popd

pushd ghci
ahc-cabal act-as-setup --build-type=Simple -- configure --builddir="$AHC_TMPDIR"/dist/ghci -fghci --ghc-option=-this-unit-id=ghci $ASTERIUS_CONFIGURE_OPTIONS
ahc-cabal act-as-setup --build-type=Simple -- build -j4 --builddir="$AHC_TMPDIR"/dist/ghci
ahc-cabal act-as-setup --build-type=Simple -- install --builddir="$AHC_TMPDIR"/dist/ghci
popd

ahc-cabal v1-install $ASTERIUS_CONFIGURE_OPTIONS \
  aeson \
  parsec

pushd asterius-prelude
ahc-cabal act-as-setup --build-type=Simple -- configure --builddir="$AHC_TMPDIR"/dist/asterius-prelude --ghc-option=-this-unit-id=asterius-prelude $ASTERIUS_CONFIGURE_OPTIONS
ahc-cabal act-as-setup --build-type=Simple -- build -j4 --builddir="$AHC_TMPDIR"/dist/asterius-prelude
ahc-cabal act-as-setup --build-type=Simple -- install --builddir="$AHC_TMPDIR"/dist/asterius-prelude
popd

popd
