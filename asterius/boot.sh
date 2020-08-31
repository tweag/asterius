#!/bin/sh -e

cp -r $ASTERIUS_BOOT_LIBS_DIR .
cd boot-libs

cd ghc-prim
$ASTERIUS_SETUP_GHC_PRIM configure --builddir=$ASTERIUS_TMP_DIR/dist/ghc-prim $ASTERIUS_CONFIGURE_OPTIONS
$ASTERIUS_SETUP_GHC_PRIM build -j --builddir=$ASTERIUS_TMP_DIR/dist/ghc-prim
$ASTERIUS_SETUP_GHC_PRIM install --builddir=$ASTERIUS_TMP_DIR/dist/ghc-prim
cd ..

cd integer-simple
ahc-cabal act-as-setup --build-type=Simple -- configure --builddir=$ASTERIUS_TMP_DIR/dist/integer-simple $ASTERIUS_CONFIGURE_OPTIONS
ahc-cabal act-as-setup --build-type=Simple -- build -j --builddir=$ASTERIUS_TMP_DIR/dist/integer-simple
ahc-cabal act-as-setup --build-type=Simple -- install --builddir=$ASTERIUS_TMP_DIR/dist/integer-simple
cd ..

cd base
autoreconf -i
ahc-cabal act-as-setup --build-type=Configure -- configure --builddir=$ASTERIUS_TMP_DIR/dist/base -finteger-simple $ASTERIUS_CONFIGURE_OPTIONS
ahc-cabal act-as-setup --build-type=Configure -- build -j --builddir=$ASTERIUS_TMP_DIR/dist/base
ahc-cabal act-as-setup --build-type=Configure -- install --builddir=$ASTERIUS_TMP_DIR/dist/base
cd ..

cd ghc-heap
ahc-cabal act-as-setup --build-type=Simple -- configure --builddir=$ASTERIUS_TMP_DIR/dist/ghc-heap $ASTERIUS_CONFIGURE_OPTIONS
ahc-cabal act-as-setup --build-type=Simple -- build -j --builddir=$ASTERIUS_TMP_DIR/dist/ghc-heap
ahc-cabal act-as-setup --build-type=Simple -- install --builddir=$ASTERIUS_TMP_DIR/dist/ghc-heap
cd ..

cd ghc-boot-th
ahc-cabal act-as-setup --build-type=Simple -- configure --builddir=$ASTERIUS_TMP_DIR/dist/ghc-boot-th $ASTERIUS_CONFIGURE_OPTIONS
ahc-cabal act-as-setup --build-type=Simple -- build -j --builddir=$ASTERIUS_TMP_DIR/dist/ghc-boot-th
ahc-cabal act-as-setup --build-type=Simple -- install --builddir=$ASTERIUS_TMP_DIR/dist/ghc-boot-th
cd ..

ahc-cabal v1-update || true
ahc-cabal v1-install $ASTERIUS_CONFIGURE_OPTIONS \
  binary \
  directory \
  mtl \
  pretty \
  unix

cd ghc-boot
ahc-cabal act-as-setup --build-type=Simple -- configure --builddir=$ASTERIUS_TMP_DIR/dist/ghc-boot $ASTERIUS_CONFIGURE_OPTIONS
ahc-cabal act-as-setup --build-type=Simple -- build -j --builddir=$ASTERIUS_TMP_DIR/dist/ghc-boot
ahc-cabal act-as-setup --build-type=Simple -- install --builddir=$ASTERIUS_TMP_DIR/dist/ghc-boot
cd ..

cd template-haskell
ahc-cabal act-as-setup --build-type=Simple -- configure --builddir=$ASTERIUS_TMP_DIR/dist/template-haskell $ASTERIUS_CONFIGURE_OPTIONS
ahc-cabal act-as-setup --build-type=Simple -- build -j --builddir=$ASTERIUS_TMP_DIR/dist/template-haskell
ahc-cabal act-as-setup --build-type=Simple -- install --builddir=$ASTERIUS_TMP_DIR/dist/template-haskell
cd ..

cd ghci
ahc-cabal act-as-setup --build-type=Simple -- configure --builddir=$ASTERIUS_TMP_DIR/dist/ghci -fghci $ASTERIUS_CONFIGURE_OPTIONS
ahc-cabal act-as-setup --build-type=Simple -- build -j --builddir=$ASTERIUS_TMP_DIR/dist/ghci
ahc-cabal act-as-setup --build-type=Simple -- install --builddir=$ASTERIUS_TMP_DIR/dist/ghci
cd ..

cd text
ahc-cabal act-as-setup --build-type=Simple -- configure --builddir=$ASTERIUS_TMP_DIR/dist/text $ASTERIUS_CONFIGURE_OPTIONS
ahc-cabal act-as-setup --build-type=Simple -- build -j --builddir=$ASTERIUS_TMP_DIR/dist/text
ahc-cabal act-as-setup --build-type=Simple -- install --builddir=$ASTERIUS_TMP_DIR/dist/text
cd ..

ahc-cabal v1-install $ASTERIUS_CONFIGURE_OPTIONS \
  aeson \
  parsec

cd asterius-prelude
ahc-cabal act-as-setup --build-type=Simple -- configure --builddir=$ASTERIUS_TMP_DIR/dist/asterius-prelude $ASTERIUS_CONFIGURE_OPTIONS
ahc-cabal act-as-setup --build-type=Simple -- build -j --builddir=$ASTERIUS_TMP_DIR/dist/asterius-prelude
ahc-cabal act-as-setup --build-type=Simple -- install --builddir=$ASTERIUS_TMP_DIR/dist/asterius-prelude
cd ..
