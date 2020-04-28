#!/bin/sh -e

set -x

cp -r $ASTERIUS_BOOT_LIBS_DIR boot-libs
chmod +w -R .
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

cd ghc-heap
ahc-cabal act-as-setup --build-type=Simple -- configure --prefix=$ASTERIUS_LIB_DIR --package-db=clear --package-db=global --builddir=$ASTERIUS_TMP_DIR/dist/ghc-heap --with-ghc=$ASTERIUS_AHC --with-ghc-pkg=$ASTERIUS_AHCPKG $ASTERIUS_CONFIGURE_OPTIONS
ahc-cabal act-as-setup --build-type=Simple -- build --builddir=$ASTERIUS_TMP_DIR/dist/ghc-heap
ahc-cabal act-as-setup --build-type=Simple -- install --builddir=$ASTERIUS_TMP_DIR/dist/ghc-heap
cd ..

cd ghc-boot-th
ahc-cabal act-as-setup --build-type=Simple -- configure --prefix=$ASTERIUS_LIB_DIR --package-db=clear --package-db=global --builddir=$ASTERIUS_TMP_DIR/dist/ghc-boot-th --with-ghc=$ASTERIUS_AHC --with-ghc-pkg=$ASTERIUS_AHCPKG $ASTERIUS_CONFIGURE_OPTIONS
ahc-cabal act-as-setup --build-type=Simple -- build --builddir=$ASTERIUS_TMP_DIR/dist/ghc-boot-th
ahc-cabal act-as-setup --build-type=Simple -- install --builddir=$ASTERIUS_TMP_DIR/dist/ghc-boot-th
cd ..

ahc-cabal v1-update || true
ahc-cabal v1-install --prefix=$ASTERIUS_LIB_DIR --package-db=clear --package-db=global $ASTERIUS_CONFIGURE_OPTIONS \
  binary \
  directory \
  mtl \
  pretty \
  unix

cd ghc-boot
ahc-cabal act-as-setup --build-type=Simple -- configure --prefix=$ASTERIUS_LIB_DIR --package-db=clear --package-db=global --builddir=$ASTERIUS_TMP_DIR/dist/ghc-boot --with-ghc=$ASTERIUS_AHC --with-ghc-pkg=$ASTERIUS_AHCPKG $ASTERIUS_CONFIGURE_OPTIONS
ahc-cabal act-as-setup --build-type=Simple -- build --builddir=$ASTERIUS_TMP_DIR/dist/ghc-boot
ahc-cabal act-as-setup --build-type=Simple -- install --builddir=$ASTERIUS_TMP_DIR/dist/ghc-boot
cd ..

cd template-haskell
ahc-cabal act-as-setup --build-type=Simple -- configure --prefix=$ASTERIUS_LIB_DIR --package-db=clear --package-db=global --builddir=$ASTERIUS_TMP_DIR/dist/template-haskell --with-ghc=$ASTERIUS_AHC --with-ghc-pkg=$ASTERIUS_AHCPKG $ASTERIUS_CONFIGURE_OPTIONS
ahc-cabal act-as-setup --build-type=Simple -- build --builddir=$ASTERIUS_TMP_DIR/dist/template-haskell
ahc-cabal act-as-setup --build-type=Simple -- install --builddir=$ASTERIUS_TMP_DIR/dist/template-haskell
cd ..

# cd ghci
# ahc-cabal act-as-setup --build-type=Simple -- configure --prefix=$ASTERIUS_LIB_DIR --package-db=clear --package-db=global --builddir=$ASTERIUS_TMP_DIR/dist/ghci --with-ghc=$ASTERIUS_AHC --with-ghc-pkg=$ASTERIUS_AHCPKG -fghci $ASTERIUS_CONFIGURE_OPTIONS
# ahc-cabal act-as-setup --build-type=Simple -- build --builddir=$ASTERIUS_TMP_DIR/dist/ghci
# ahc-cabal act-as-setup --build-type=Simple -- install --builddir=$ASTERIUS_TMP_DIR/dist/ghci
# cd ..

cd text
ahc-cabal act-as-setup --build-type=Simple -- configure --prefix=$ASTERIUS_LIB_DIR --package-db=clear --package-db=global --builddir=$ASTERIUS_TMP_DIR/dist/text --with-ghc=$ASTERIUS_AHC --with-ghc-pkg=$ASTERIUS_AHCPKG --ghc-option=-DASTERIUS $ASTERIUS_CONFIGURE_OPTIONS
ahc-cabal act-as-setup --build-type=Simple -- build --builddir=$ASTERIUS_TMP_DIR/dist/text
ahc-cabal act-as-setup --build-type=Simple -- install --builddir=$ASTERIUS_TMP_DIR/dist/text
cd ..

ahc-cabal v1-install --prefix=$ASTERIUS_LIB_DIR --package-db=clear --package-db=global $ASTERIUS_CONFIGURE_OPTIONS \
  aeson \
  parsec

cd asterius-prelude
ahc-cabal act-as-setup --build-type=Simple -- configure --prefix=$ASTERIUS_LIB_DIR --package-db=clear --package-db=global --builddir=$ASTERIUS_TMP_DIR/dist/asterius-prelude --with-ghc=$ASTERIUS_AHC --with-ghc-pkg=$ASTERIUS_AHCPKG $ASTERIUS_CONFIGURE_OPTIONS
ahc-cabal act-as-setup --build-type=Simple -- build --builddir=$ASTERIUS_TMP_DIR/dist/asterius-prelude
ahc-cabal act-as-setup --build-type=Simple -- install --builddir=$ASTERIUS_TMP_DIR/dist/asterius-prelude
cd ..
