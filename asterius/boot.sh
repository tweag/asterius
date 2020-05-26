#!/bin/bash

set -euo pipefail

cp -r $ASTERIUS_BOOT_LIBS_DIR .
cd boot-libs

cd ghc-prim
echo $ASTERIUS_CONFIGURE_OPTIONS | xargs $ASTERIUS_SETUP_GHC_PRIM configure --prefix=$ASTERIUS_LIB_DIR --package-db=clear --package-db=global --builddir=$ASTERIUS_TMP_DIR/dist/ghc-prim --with-ghc=$ASTERIUS_AHC --with-ghc-pkg=$ASTERIUS_AHCPKG --with-ar=$ASTERIUS_AR
$ASTERIUS_SETUP_GHC_PRIM build --builddir=$ASTERIUS_TMP_DIR/dist/ghc-prim
$ASTERIUS_SETUP_GHC_PRIM install --builddir=$ASTERIUS_TMP_DIR/dist/ghc-prim
cd ..

cd integer-simple
echo $ASTERIUS_CONFIGURE_OPTIONS | xargs ahc-cabal act-as-setup --build-type=Simple -- configure --prefix=$ASTERIUS_LIB_DIR --package-db=clear --package-db=global --builddir=$ASTERIUS_TMP_DIR/dist/integer-simple --with-ghc=$ASTERIUS_AHC --with-ghc-pkg=$ASTERIUS_AHCPKG --with-ar=$ASTERIUS_AR
ahc-cabal act-as-setup --build-type=Simple -- build --builddir=$ASTERIUS_TMP_DIR/dist/integer-simple
ahc-cabal act-as-setup --build-type=Simple -- install --builddir=$ASTERIUS_TMP_DIR/dist/integer-simple
cd ..

cd base
autoreconf -i
echo $ASTERIUS_CONFIGURE_OPTIONS | xargs ahc-cabal act-as-setup --build-type=Configure -- configure --prefix=$ASTERIUS_LIB_DIR --package-db=clear --package-db=global --builddir=$ASTERIUS_TMP_DIR/dist/base --with-ghc=$ASTERIUS_AHC --with-ghc-pkg=$ASTERIUS_AHCPKG --with-ar=$ASTERIUS_AR -finteger-simple --ghc-option=-DASTERIUS
ahc-cabal act-as-setup --build-type=Configure -- build --builddir=$ASTERIUS_TMP_DIR/dist/base
ahc-cabal act-as-setup --build-type=Configure -- install --builddir=$ASTERIUS_TMP_DIR/dist/base
cd ..

cd ghc-heap
echo $ASTERIUS_CONFIGURE_OPTIONS | xargs ahc-cabal act-as-setup --build-type=Simple -- configure --prefix=$ASTERIUS_LIB_DIR --package-db=clear --package-db=global --builddir=$ASTERIUS_TMP_DIR/dist/ghc-heap --with-ghc=$ASTERIUS_AHC --with-ghc-pkg=$ASTERIUS_AHCPKG --with-ar=$ASTERIUS_AR
ahc-cabal act-as-setup --build-type=Simple -- build --builddir=$ASTERIUS_TMP_DIR/dist/ghc-heap
ahc-cabal act-as-setup --build-type=Simple -- install --builddir=$ASTERIUS_TMP_DIR/dist/ghc-heap
cd ..

cd ghc-boot-th
echo $ASTERIUS_CONFIGURE_OPTIONS | xargs ahc-cabal act-as-setup --build-type=Simple -- configure --prefix=$ASTERIUS_LIB_DIR --package-db=clear --package-db=global --builddir=$ASTERIUS_TMP_DIR/dist/ghc-boot-th --with-ghc=$ASTERIUS_AHC --with-ghc-pkg=$ASTERIUS_AHCPKG --with-ar=$ASTERIUS_AR
ahc-cabal act-as-setup --build-type=Simple -- build --builddir=$ASTERIUS_TMP_DIR/dist/ghc-boot-th
ahc-cabal act-as-setup --build-type=Simple -- install --builddir=$ASTERIUS_TMP_DIR/dist/ghc-boot-th
cd ..

ahc-cabal v1-update || true
echo $ASTERIUS_CONFIGURE_OPTIONS | xargs ahc-cabal v1-install --prefix=$ASTERIUS_LIB_DIR --package-db=clear --package-db=global \
  binary \
  directory \
  mtl \
  pretty \
  unix

cd ghc-boot
echo $ASTERIUS_CONFIGURE_OPTIONS | xargs ahc-cabal act-as-setup --build-type=Simple -- configure --prefix=$ASTERIUS_LIB_DIR --package-db=clear --package-db=global --builddir=$ASTERIUS_TMP_DIR/dist/ghc-boot --with-ghc=$ASTERIUS_AHC --with-ghc-pkg=$ASTERIUS_AHCPKG --with-ar=$ASTERIUS_AR
ahc-cabal act-as-setup --build-type=Simple -- build --builddir=$ASTERIUS_TMP_DIR/dist/ghc-boot
ahc-cabal act-as-setup --build-type=Simple -- install --builddir=$ASTERIUS_TMP_DIR/dist/ghc-boot
cd ..

cd template-haskell
echo $ASTERIUS_CONFIGURE_OPTIONS | xargs ahc-cabal act-as-setup --build-type=Simple -- configure --prefix=$ASTERIUS_LIB_DIR --package-db=clear --package-db=global --builddir=$ASTERIUS_TMP_DIR/dist/template-haskell --with-ghc=$ASTERIUS_AHC --with-ghc-pkg=$ASTERIUS_AHCPKG --with-ar=$ASTERIUS_AR
ahc-cabal act-as-setup --build-type=Simple -- build --builddir=$ASTERIUS_TMP_DIR/dist/template-haskell
ahc-cabal act-as-setup --build-type=Simple -- install --builddir=$ASTERIUS_TMP_DIR/dist/template-haskell
cd ..

cd ghci
echo $ASTERIUS_CONFIGURE_OPTIONS | xargs ahc-cabal act-as-setup --build-type=Simple -- configure --prefix=$ASTERIUS_LIB_DIR --package-db=clear --package-db=global --builddir=$ASTERIUS_TMP_DIR/dist/ghci --with-ghc=$ASTERIUS_AHC --with-ghc-pkg=$ASTERIUS_AHCPKG --with-ar=$ASTERIUS_AR -fghci
ahc-cabal act-as-setup --build-type=Simple -- build --builddir=$ASTERIUS_TMP_DIR/dist/ghci
ahc-cabal act-as-setup --build-type=Simple -- install --builddir=$ASTERIUS_TMP_DIR/dist/ghci
cd ..

cd text
echo $ASTERIUS_CONFIGURE_OPTIONS | xargs ahc-cabal act-as-setup --build-type=Simple -- configure --prefix=$ASTERIUS_LIB_DIR --package-db=clear --package-db=global --builddir=$ASTERIUS_TMP_DIR/dist/text --with-ghc=$ASTERIUS_AHC --with-ghc-pkg=$ASTERIUS_AHCPKG --with-ar=$ASTERIUS_AR --ghc-option=-DASTERIUS
ahc-cabal act-as-setup --build-type=Simple -- build --builddir=$ASTERIUS_TMP_DIR/dist/text
ahc-cabal act-as-setup --build-type=Simple -- install --builddir=$ASTERIUS_TMP_DIR/dist/text
cd ..

echo $ASTERIUS_CONFIGURE_OPTIONS | xargs ahc-cabal v1-install --prefix=$ASTERIUS_LIB_DIR --package-db=clear --package-db=global \
  aeson \
  parsec

cd asterius-prelude
echo $ASTERIUS_CONFIGURE_OPTIONS | xargs ahc-cabal act-as-setup --build-type=Simple -- configure --prefix=$ASTERIUS_LIB_DIR --package-db=clear --package-db=global --builddir=$ASTERIUS_TMP_DIR/dist/asterius-prelude --with-ghc=$ASTERIUS_AHC --with-ghc-pkg=$ASTERIUS_AHCPKG --with-ar=$ASTERIUS_AR
ahc-cabal act-as-setup --build-type=Simple -- build --builddir=$ASTERIUS_TMP_DIR/dist/asterius-prelude
ahc-cabal act-as-setup --build-type=Simple -- install --builddir=$ASTERIUS_TMP_DIR/dist/asterius-prelude
cd ..
