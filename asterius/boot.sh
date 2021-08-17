#!/usr/bin/env bash

set -euo pipefail

cp -r $ASTERIUS_BOOT_LIBS_DIR .
pushd boot-libs

pushd ghc-prim
$ASTERIUS_SETUP_GHC_PRIM configure --builddir=$AHC_TMPDIR/dist/ghc-prim $ASTERIUS_CONFIGURE_OPTIONS
$ASTERIUS_SETUP_GHC_PRIM build -j --builddir=$AHC_TMPDIR/dist/ghc-prim
$ASTERIUS_SETUP_GHC_PRIM install --builddir=$AHC_TMPDIR/dist/ghc-prim
popd

pushd integer-simple
ahc-cabal act-as-setup --build-type=Simple -- configure --builddir=$AHC_TMPDIR/dist/integer-simple $ASTERIUS_CONFIGURE_OPTIONS
ahc-cabal act-as-setup --build-type=Simple -- build -j --builddir=$AHC_TMPDIR/dist/integer-simple
ahc-cabal act-as-setup --build-type=Simple -- install --builddir=$AHC_TMPDIR/dist/integer-simple
popd

pushd base
autoreconf -i
ahc-cabal act-as-setup --build-type=Configure -- configure --builddir=$AHC_TMPDIR/dist/base -finteger-simple $ASTERIUS_CONFIGURE_OPTIONS
ahc-cabal act-as-setup --build-type=Configure -- build -j --builddir=$AHC_TMPDIR/dist/base
ahc-cabal act-as-setup --build-type=Configure -- install --builddir=$AHC_TMPDIR/dist/base
popd

pushd ghc-heap
ahc-cabal act-as-setup --build-type=Simple -- configure --builddir=$AHC_TMPDIR/dist/ghc-heap $ASTERIUS_CONFIGURE_OPTIONS
ahc-cabal act-as-setup --build-type=Simple -- build -j --builddir=$AHC_TMPDIR/dist/ghc-heap
ahc-cabal act-as-setup --build-type=Simple -- install --builddir=$AHC_TMPDIR/dist/ghc-heap
popd

pushd ghc-boot-th
ahc-cabal act-as-setup --build-type=Simple -- configure --builddir=$AHC_TMPDIR/dist/ghc-boot-th $ASTERIUS_CONFIGURE_OPTIONS
ahc-cabal act-as-setup --build-type=Simple -- build -j --builddir=$AHC_TMPDIR/dist/ghc-boot-th
ahc-cabal act-as-setup --build-type=Simple -- install --builddir=$AHC_TMPDIR/dist/ghc-boot-th
popd

ahc-cabal v1-update || true

ahc-cabal v1-install --only-dependencies $ASTERIUS_CONFIGURE_OPTIONS \
  unix

pushd $(mktemp -d)
ahc-cabal get unix-2.7.2.2
cd unix-2.7.2.2
ahc-cabal act-as-setup --build-type=Configure -- configure --builddir=$AHC_TMPDIR/dist/unix --ghc-option=-this-unit-id=unix $ASTERIUS_CONFIGURE_OPTIONS
ahc-cabal act-as-setup --build-type=Configure -- build -j --builddir=$AHC_TMPDIR/dist/unix
ahc-cabal act-as-setup --build-type=Configure -- install --builddir=$AHC_TMPDIR/dist/unix
popd

ahc-cabal v1-install $ASTERIUS_CONFIGURE_OPTIONS \
  binary \
  directory \
  mtl \
  pretty

pushd ghc-boot
ahc-cabal act-as-setup --build-type=Simple -- configure --builddir=$AHC_TMPDIR/dist/ghc-boot $ASTERIUS_CONFIGURE_OPTIONS
ahc-cabal act-as-setup --build-type=Simple -- build -j --builddir=$AHC_TMPDIR/dist/ghc-boot
ahc-cabal act-as-setup --build-type=Simple -- install --builddir=$AHC_TMPDIR/dist/ghc-boot
popd

pushd template-haskell
ahc-cabal act-as-setup --build-type=Simple -- configure --builddir=$AHC_TMPDIR/dist/template-haskell $ASTERIUS_CONFIGURE_OPTIONS
ahc-cabal act-as-setup --build-type=Simple -- build -j --builddir=$AHC_TMPDIR/dist/template-haskell
ahc-cabal act-as-setup --build-type=Simple -- install --builddir=$AHC_TMPDIR/dist/template-haskell
popd

pushd ghci
ahc-cabal act-as-setup --build-type=Simple -- configure --builddir=$AHC_TMPDIR/dist/ghci -fghci --ghc-option=-this-unit-id=ghci $ASTERIUS_CONFIGURE_OPTIONS
ahc-cabal act-as-setup --build-type=Simple -- build -j --builddir=$AHC_TMPDIR/dist/ghci
ahc-cabal act-as-setup --build-type=Simple -- install --builddir=$AHC_TMPDIR/dist/ghci
popd

ahc-cabal v1-install $ASTERIUS_CONFIGURE_OPTIONS \
  aeson \
  parsec

pushd asterius-prelude
ahc-cabal act-as-setup --build-type=Simple -- configure --builddir=$AHC_TMPDIR/dist/asterius-prelude --ghc-option=-this-unit-id=asterius-prelude $ASTERIUS_CONFIGURE_OPTIONS
ahc-cabal act-as-setup --build-type=Simple -- build -j --builddir=$AHC_TMPDIR/dist/asterius-prelude
ahc-cabal act-as-setup --build-type=Simple -- install --builddir=$AHC_TMPDIR/dist/asterius-prelude
popd

popd
