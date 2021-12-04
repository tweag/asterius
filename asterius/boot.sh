#!/usr/bin/env bash

set -euox pipefail

AHC_TMPDIR=$(mktemp -d)
export AHC_TMPDIR

cp -r "$AHC_SRCDIR"/ghc-toolkit/boot-libs/* "$GHC_ASTERIUS_BOOT"/* "$AHC_TMPDIR"
chmod u+w -R "$AHC_TMPDIR"

rm -rf "$AHC_LIBDIR"
mkdir -p "$AHC_LIBDIR"

cp -r \
  "$GHC_ASTERIUS"/ghc-asterius/autogen \
  "$AHC_LIBDIR"/include
cp \
  "$GHC_ASTERIUS_BOOT"/llvm-passes \
  "$GHC_ASTERIUS_BOOT"/llvm-targets \
  "$GHC_ASTERIUS_BOOT"/platformConstants \
  "$GHC_ASTERIUS_BOOT"/settings \
  "$GHC_ASTERIUS_BOOT"/template-hsc.h \
  "$AHC_LIBDIR"
chmod u+w -R "$AHC_LIBDIR"

mkdir "$AHC_LIBDIR"/package.conf.d
cp "$AHC_SRCDIR"/ghc-toolkit/boot-libs/rts/rts.conf "$AHC_LIBDIR"/package.conf.d
ahc-pkg --global recache

mkdir "$AHC_LIBDIR"/rts
find "$AHC_TMPDIR"/rts -name '*.cmm' -print0 | xargs -0 -n1 sh -c 'ahc -c -O2 -dcmm-lint -I"$AHC_LIBDIR"/include -this-unit-id rts -o "$AHC_TMPDIR"/rts/$(basename "$0" .cmm).o "$0"'
ar qDS "$AHC_LIBDIR"/rts/libHSrts.a "$AHC_TMPDIR"/rts/*.o

pushd "$AHC_TMPDIR"

ASTERIUS_CONFIGURE_OPTIONS="--disable-shared --disable-profiling --disable-debug-info --disable-library-for-ghci --disable-split-objs --disable-split-sections --disable-library-stripping --disable-relocatable -O2 --prefix=$AHC_LIBDIR --global --ipid=\$pkg --with-compiler=ahc --with-hc-pkg=ahc-pkg --with-ar=ar --hsc2hs-option=--cross-compile --ghc-option=-v1 --ghc-option=-dsuppress-ticks"

pushd ghc-prim
Setup-ghc-prim configure $ASTERIUS_CONFIGURE_OPTIONS
Setup-ghc-prim build -j
Setup-ghc-prim install
popd

pushd integer-simple
ahc-cabal act-as-setup --build-type=Simple -- configure $ASTERIUS_CONFIGURE_OPTIONS
ahc-cabal act-as-setup --build-type=Simple -- build -j
ahc-cabal act-as-setup --build-type=Simple -- install
popd

pushd base
CFLAGS=-I$AHC_TMPDIR/base ahc-cabal act-as-setup --build-type=Configure -- configure -finteger-simple $ASTERIUS_CONFIGURE_OPTIONS
ahc-cabal act-as-setup --build-type=Configure -- build -j
ahc-cabal act-as-setup --build-type=Configure -- install
popd

pushd array
ahc-cabal act-as-setup --build-type=Simple -- configure $ASTERIUS_CONFIGURE_OPTIONS
ahc-cabal act-as-setup --build-type=Simple -- build -j
ahc-cabal act-as-setup --build-type=Simple -- install
popd

pushd deepseq
ahc-cabal act-as-setup --build-type=Simple -- configure $ASTERIUS_CONFIGURE_OPTIONS
ahc-cabal act-as-setup --build-type=Simple -- build -j
ahc-cabal act-as-setup --build-type=Simple -- install
popd

pushd bytestring
ahc-cabal act-as-setup --build-type=Simple -- configure $ASTERIUS_CONFIGURE_OPTIONS
ahc-cabal act-as-setup --build-type=Simple -- build -j
ahc-cabal act-as-setup --build-type=Simple -- install
popd

pushd time
ahc-cabal act-as-setup --build-type=Configure -- configure $ASTERIUS_CONFIGURE_OPTIONS
ahc-cabal act-as-setup --build-type=Configure -- build -j
ahc-cabal act-as-setup --build-type=Configure -- install
popd

pushd unix
ahc-cabal act-as-setup --build-type=Configure -- configure --ghc-option=-this-unit-id=unix $ASTERIUS_CONFIGURE_OPTIONS
ahc-cabal act-as-setup --build-type=Configure -- build -j
ahc-cabal act-as-setup --build-type=Configure -- install
popd

pushd filepath
ahc-cabal act-as-setup --build-type=Simple -- configure --ghc-option=-this-unit-id=filepath $ASTERIUS_CONFIGURE_OPTIONS
ahc-cabal act-as-setup --build-type=Simple -- build -j
ahc-cabal act-as-setup --build-type=Simple -- install
popd

pushd directory
ahc-cabal act-as-setup --build-type=Configure -- configure --ghc-option=-this-unit-id=directory $ASTERIUS_CONFIGURE_OPTIONS
ahc-cabal act-as-setup --build-type=Configure -- build -j
ahc-cabal act-as-setup --build-type=Configure -- install
popd

pushd ghc-heap
ahc-cabal act-as-setup --build-type=Simple -- configure $ASTERIUS_CONFIGURE_OPTIONS
ahc-cabal act-as-setup --build-type=Simple -- build -j
ahc-cabal act-as-setup --build-type=Simple -- install
popd

pushd ghc-boot-th
ahc-cabal act-as-setup --build-type=Simple -- configure $ASTERIUS_CONFIGURE_OPTIONS
ahc-cabal act-as-setup --build-type=Simple -- build -j
ahc-cabal act-as-setup --build-type=Simple -- install
popd

ahc-cabal update || true

ahc-cabal v1-install $ASTERIUS_CONFIGURE_OPTIONS \
  binary-0.8.8.0 \
  mtl-2.2.2 \
  pretty-1.1.3.6

pushd ghc-boot
ahc-cabal act-as-setup --build-type=Simple -- configure $ASTERIUS_CONFIGURE_OPTIONS
ahc-cabal act-as-setup --build-type=Simple -- build -j
ahc-cabal act-as-setup --build-type=Simple -- install
popd

pushd template-haskell
ahc-cabal act-as-setup --build-type=Simple -- configure $ASTERIUS_CONFIGURE_OPTIONS
ahc-cabal act-as-setup --build-type=Simple -- build -j
ahc-cabal act-as-setup --build-type=Simple -- install
popd

pushd ghci
ahc-cabal act-as-setup --build-type=Simple -- configure -fghci --ghc-option=-this-unit-id=ghci $ASTERIUS_CONFIGURE_OPTIONS
ahc-cabal act-as-setup --build-type=Simple -- build -j
ahc-cabal act-as-setup --build-type=Simple -- install
popd

ahc-cabal v1-install $ASTERIUS_CONFIGURE_OPTIONS \
  aeson-1.5.6.0 \
  parsec-3.1.14.0

pushd asterius-prelude
ahc-cabal act-as-setup --build-type=Simple -- configure --ghc-option=-this-unit-id=asterius-prelude $ASTERIUS_CONFIGURE_OPTIONS
ahc-cabal act-as-setup --build-type=Simple -- build -j
ahc-cabal act-as-setup --build-type=Simple -- install
popd

popd
