#!/bin/sh -e

cp -r $ASTERIUS_BOOT_LIBS_DIR .
cd boot-libs

cd ghc-prim
$ASTERIUS_SETUP_GHC_PRIM configure --prefix=$ASTERIUS_LIB_DIR --package-db=clear --package-db=global --builddir=$ASTERIUS_TMP_DIR/dist/ghc-prim --with-ghc=$ASTERIUS_AHC --with-ghc-pkg=$ASTERIUS_AHCPKG $ASTERIUS_CONFIGURE_OPTIONS
$ASTERIUS_SETUP_GHC_PRIM build --builddir=$ASTERIUS_TMP_DIR/dist/ghc-prim
$ASTERIUS_SETUP_GHC_PRIM install --builddir=$ASTERIUS_TMP_DIR/dist/ghc-prim
cd ..

cd integer-simple
ahc-cabal act-as-setup --build-type=Simple -- configure --prefix=$ASTERIUS_LIB_DIR --package-db=clear --package-db=global --builddir=$ASTERIUS_TMP_DIR/dist/integer-simple --with-ghc=$ASTERIUS_AHC --with-ghc-pkg=$ASTERIUS_AHCPKG $ASTERIUS_CONFIGURE_OPTIONS
ahc-cabal act-as-setup --build-type=Simple -- build --builddir=$ASTERIUS_TMP_DIR/dist/integer-simple
ahc-cabal act-as-setup --build-type=Simple -- install --builddir=$ASTERIUS_TMP_DIR/dist/integer-simple
cd ..

cd base
autoreconf -i
ahc-cabal act-as-setup --build-type=Configure -- configure --prefix=$ASTERIUS_LIB_DIR --package-db=clear --package-db=global --builddir=$ASTERIUS_TMP_DIR/dist/base --with-ghc=$ASTERIUS_AHC --with-ghc-pkg=$ASTERIUS_AHCPKG -finteger-simple --ghc-option=-DASTERIUS $ASTERIUS_CONFIGURE_OPTIONS
ahc-cabal act-as-setup --build-type=Configure -- build --builddir=$ASTERIUS_TMP_DIR/dist/base
ahc-cabal act-as-setup --build-type=Configure -- install --builddir=$ASTERIUS_TMP_DIR/dist/base
cd ..

ahc-cabal v1-update || true
ahc-cabal v1-install --prefix=$ASTERIUS_LIB_DIR --package-db=clear --package-db=global $ASTERIUS_CONFIGURE_OPTIONS \
  binary \
  directory \
  ghc-boot-th/ \
  ghc-boot/ \
  ghc-heap/ \
  mtl \
  pretty \
  unix

cd template-haskell
ahc-cabal act-as-setup --build-type=Simple -- configure --prefix=$ASTERIUS_LIB_DIR --package-db=clear --package-db=global --builddir=$ASTERIUS_TMP_DIR/dist/template-haskell --with-ghc=$ASTERIUS_AHC --with-ghc-pkg=$ASTERIUS_AHCPKG $ASTERIUS_CONFIGURE_OPTIONS
ahc-cabal act-as-setup --build-type=Simple -- build --builddir=$ASTERIUS_TMP_DIR/dist/template-haskell
ahc-cabal act-as-setup --build-type=Simple -- install --builddir=$ASTERIUS_TMP_DIR/dist/template-haskell
cd ..

ahc-cabal v1-install --prefix=$ASTERIUS_LIB_DIR --package-db=clear --package-db=global $ASTERIUS_CONFIGURE_OPTIONS \
  asterius-prelude/ \
  ghci/ \
  parsec \
  text/
