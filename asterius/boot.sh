#!/usr/bin/env bash

set -euox pipefail

AHC_TMPDIR=$(mktemp -d)
export AHC_TMPDIR

cp -r "$AHC_BOOT_SRCDIR"/* "$GHC_ASTERIUS_BOOT"/* "$AHC_TMPDIR"
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
cp "$AHC_BOOT_SRCDIR"/rts/rts.conf "$AHC_LIBDIR"/package.conf.d
ahc-pkg --global recache

mkdir "$AHC_LIBDIR"/rts
find "$AHC_TMPDIR"/rts -name '*.cmm' -exec sh -c 'ahc -c -O2 -dcmm-lint -I"$AHC_LIBDIR"/include -this-unit-id rts -o "$AHC_TMPDIR"/rts/$(basename "$0" .cmm).o "$0"' {} \;
ar qDS "$AHC_LIBDIR"/rts/libHSrts.a "$AHC_TMPDIR"/rts/*.o

pushd "$AHC_TMPDIR"

ASTERIUS_CONFIGURE_OPTIONS="--disable-shared --disable-profiling --disable-debug-info --disable-library-for-ghci --disable-split-objs --disable-split-sections --disable-library-stripping --disable-relocatable -O2 --prefix=$AHC_LIBDIR --global --ipid=\$pkg --with-compiler=ahc --with-hc-pkg=ahc-pkg --hsc2hs-option=--cross-safe --ghc-option=-v1 --ghc-option=-dsuppress-ticks"

pushd ghc-prim
Setup-ghc-prim configure $ASTERIUS_CONFIGURE_OPTIONS
Setup-ghc-prim build -j4
Setup-ghc-prim install
popd

pushd integer-simple
ahc-cabal act-as-setup --build-type=Simple -- configure $ASTERIUS_CONFIGURE_OPTIONS
ahc-cabal act-as-setup --build-type=Simple -- build -j4
ahc-cabal act-as-setup --build-type=Simple -- install
popd

pushd base
CFLAGS=-I$AHC_TMPDIR/base ahc-cabal act-as-setup --build-type=Configure -- configure -finteger-simple $ASTERIUS_CONFIGURE_OPTIONS
ahc-cabal act-as-setup --build-type=Configure -- build -j4
ahc-cabal act-as-setup --build-type=Configure -- install
popd

exit

pushd ghc-heap
ahc-cabal act-as-setup --build-type=Simple -- configure $ASTERIUS_CONFIGURE_OPTIONS
ahc-cabal act-as-setup --build-type=Simple -- build -j4
ahc-cabal act-as-setup --build-type=Simple -- install
popd

pushd ghc-boot-th
ahc-cabal act-as-setup --build-type=Simple -- configure $ASTERIUS_CONFIGURE_OPTIONS
ahc-cabal act-as-setup --build-type=Simple -- build -j4
ahc-cabal act-as-setup --build-type=Simple -- install
popd

ahc-cabal v1-update || true

ahc-cabal v1-install --only-dependencies $ASTERIUS_CONFIGURE_OPTIONS \
  unix

pushd "$(mktemp -d)"
ahc-cabal get unix-2.7.2.2
cd unix-2.7.2.2
ahc-cabal act-as-setup --build-type=Configure -- configure --ghc-option=-this-unit-id=unix $ASTERIUS_CONFIGURE_OPTIONS
ahc-cabal act-as-setup --build-type=Configure -- build -j4
ahc-cabal act-as-setup --build-type=Configure -- install
popd

ahc-cabal v1-install $ASTERIUS_CONFIGURE_OPTIONS \
  binary \
  directory \
  mtl \
  pretty

pushd ghc-boot
ahc-cabal act-as-setup --build-type=Simple -- configure $ASTERIUS_CONFIGURE_OPTIONS
ahc-cabal act-as-setup --build-type=Simple -- build -j4
ahc-cabal act-as-setup --build-type=Simple -- install
popd

pushd template-haskell
ahc-cabal act-as-setup --build-type=Simple -- configure $ASTERIUS_CONFIGURE_OPTIONS
ahc-cabal act-as-setup --build-type=Simple -- build -j4
ahc-cabal act-as-setup --build-type=Simple -- install
popd

pushd ghci
ahc-cabal act-as-setup --build-type=Simple -- configure -fghci --ghc-option=-this-unit-id=ghci $ASTERIUS_CONFIGURE_OPTIONS
ahc-cabal act-as-setup --build-type=Simple -- build -j4
ahc-cabal act-as-setup --build-type=Simple -- install
popd

ahc-cabal v1-install $ASTERIUS_CONFIGURE_OPTIONS \
  aeson \
  parsec

pushd asterius-prelude
ahc-cabal act-as-setup --build-type=Simple -- configure --ghc-option=-this-unit-id=asterius-prelude $ASTERIUS_CONFIGURE_OPTIONS
ahc-cabal act-as-setup --build-type=Simple -- build -j4
ahc-cabal act-as-setup --build-type=Simple -- install
popd

popd
