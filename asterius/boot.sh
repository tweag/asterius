#!/bin/sh -e

cp -r $ASTERIUS_BOOT_LIBS_DIR .
cd boot-libs

$ASTERIUS_GHC ghc-prim/Setup.hs -no-keep-hi-files -no-keep-o-files -threaded -rtsopts -with-rtsopts="-I0 -qg -qb" -o $ASTERIUS_TMP_DIR/Setup-ghc-prim
$ASTERIUS_GHC integer-simple/Setup.hs -no-keep-hi-files -no-keep-o-files -threaded -rtsopts -with-rtsopts="-I0 -qg -qb" -o $ASTERIUS_TMP_DIR/Setup-simple
$ASTERIUS_GHC base/Setup.hs -no-keep-hi-files -no-keep-o-files -threaded -rtsopts -with-rtsopts="-I0 -qg -qb" -o $ASTERIUS_TMP_DIR/Setup-autoconf

cd ghc-prim
autoreconf -i
$ASTERIUS_TMP_DIR/Setup-ghc-prim configure --prefix=$ASTERIUS_LIB_DIR --package-db=clear --package-db=global --builddir=$ASTERIUS_TMP_DIR/dist/ghc-prim --with-ghc=$ASTERIUS_AHC --with-ghc-pkg=$ASTERIUS_AHCPKG $ASTERIUS_CONFIGURE_OPTIONS
$ASTERIUS_TMP_DIR/Setup-ghc-prim build --builddir=$ASTERIUS_TMP_DIR/dist/ghc-prim $ASTERIUS_BUILD_OPTIONS
$ASTERIUS_TMP_DIR/Setup-ghc-prim install --builddir=$ASTERIUS_TMP_DIR/dist/ghc-prim $ASTERIUS_INSTALL_OPTIONS
cd ..

cd integer-simple
$ASTERIUS_TMP_DIR/Setup-simple configure --prefix=$ASTERIUS_LIB_DIR --package-db=clear --package-db=global --builddir=$ASTERIUS_TMP_DIR/dist/integer-simple --with-ghc=$ASTERIUS_AHC --with-ghc-pkg=$ASTERIUS_AHCPKG $ASTERIUS_CONFIGURE_OPTIONS
$ASTERIUS_TMP_DIR/Setup-simple build --builddir=$ASTERIUS_TMP_DIR/dist/integer-simple $ASTERIUS_BUILD_OPTIONS
$ASTERIUS_TMP_DIR/Setup-simple install --builddir=$ASTERIUS_TMP_DIR/dist/integer-simple $ASTERIUS_INSTALL_OPTIONS
cd ..

cd base
autoreconf -i
$ASTERIUS_TMP_DIR/Setup-autoconf configure --prefix=$ASTERIUS_LIB_DIR --package-db=clear --package-db=global --builddir=$ASTERIUS_TMP_DIR/dist/base --with-ghc=$ASTERIUS_AHC --with-ghc-pkg=$ASTERIUS_AHCPKG -finteger-simple --ghc-option=-DASTERIUS $ASTERIUS_CONFIGURE_OPTIONS
$ASTERIUS_TMP_DIR/Setup-autoconf build --builddir=$ASTERIUS_TMP_DIR/dist/base $ASTERIUS_BUILD_OPTIONS
$ASTERIUS_TMP_DIR/Setup-autoconf install --builddir=$ASTERIUS_TMP_DIR/dist/base $ASTERIUS_INSTALL_OPTIONS
cd ..

cd ghc-heap
$ASTERIUS_TMP_DIR/Setup-simple configure --prefix=$ASTERIUS_LIB_DIR --package-db=clear --package-db=global --builddir=$ASTERIUS_TMP_DIR/dist/ghc-heap --with-ghc=$ASTERIUS_AHC --with-ghc-pkg=$ASTERIUS_AHCPKG $ASTERIUS_CONFIGURE_OPTIONS
$ASTERIUS_TMP_DIR/Setup-simple build --builddir=$ASTERIUS_TMP_DIR/dist/ghc-heap $ASTERIUS_BUILD_OPTIONS
$ASTERIUS_TMP_DIR/Setup-simple install --builddir=$ASTERIUS_TMP_DIR/dist/ghc-heap $ASTERIUS_INSTALL_OPTIONS
cd ..

ahc-cabal v1-update || true
ahc-cabal v1-install --prefix=$ASTERIUS_LIB_DIR --package-db=clear --package-db=global $ASTERIUS_CONFIGURE_OPTIONS \
  ghc-boot \
  mtl \
  pretty \
  unix

cd template-haskell
$ASTERIUS_TMP_DIR/Setup-simple configure --prefix=$ASTERIUS_LIB_DIR --package-db=clear --package-db=global --builddir=$ASTERIUS_TMP_DIR/dist/template-haskell --with-ghc=$ASTERIUS_AHC --with-ghc-pkg=$ASTERIUS_AHCPKG $ASTERIUS_CONFIGURE_OPTIONS
$ASTERIUS_TMP_DIR/Setup-simple build --builddir=$ASTERIUS_TMP_DIR/dist/template-haskell $ASTERIUS_BUILD_OPTIONS
$ASTERIUS_TMP_DIR/Setup-simple install --builddir=$ASTERIUS_TMP_DIR/dist/template-haskell $ASTERIUS_INSTALL_OPTIONS
cd ..

cd ghci
$ASTERIUS_TMP_DIR/Setup-simple configure --prefix=$ASTERIUS_LIB_DIR --package-db=clear --package-db=global --builddir=$ASTERIUS_TMP_DIR/dist/ghci --with-ghc=$ASTERIUS_AHC --with-ghc-pkg=$ASTERIUS_AHCPKG -fghci $ASTERIUS_CONFIGURE_OPTIONS
$ASTERIUS_TMP_DIR/Setup-simple build --builddir=$ASTERIUS_TMP_DIR/dist/ghci $ASTERIUS_BUILD_OPTIONS
$ASTERIUS_TMP_DIR/Setup-simple install --builddir=$ASTERIUS_TMP_DIR/dist/ghci $ASTERIUS_INSTALL_OPTIONS
cd ..

cd text
$ASTERIUS_TMP_DIR/Setup-simple configure --prefix=$ASTERIUS_LIB_DIR --package-db=clear --package-db=global --builddir=$ASTERIUS_TMP_DIR/dist/text --with-ghc=$ASTERIUS_AHC --with-ghc-pkg=$ASTERIUS_AHCPKG --ghc-option=-DASTERIUS $ASTERIUS_CONFIGURE_OPTIONS
$ASTERIUS_TMP_DIR/Setup-simple build --builddir=$ASTERIUS_TMP_DIR/dist/text $ASTERIUS_BUILD_OPTIONS
$ASTERIUS_TMP_DIR/Setup-simple install --builddir=$ASTERIUS_TMP_DIR/dist/text $ASTERIUS_INSTALL_OPTIONS
cd ..

ahc-cabal v1-install --prefix=$ASTERIUS_LIB_DIR --package-db=clear --package-db=global $ASTERIUS_CONFIGURE_OPTIONS \
  aeson \
  parsec

cd asterius-prelude
$ASTERIUS_TMP_DIR/Setup-simple configure --prefix=$ASTERIUS_LIB_DIR --package-db=clear --package-db=global --builddir=$ASTERIUS_TMP_DIR/dist/asterius-prelude --with-ghc=$ASTERIUS_AHC --with-ghc-pkg=$ASTERIUS_AHCPKG $ASTERIUS_CONFIGURE_OPTIONS
$ASTERIUS_TMP_DIR/Setup-simple build --builddir=$ASTERIUS_TMP_DIR/dist/asterius-prelude $ASTERIUS_BUILD_OPTIONS
$ASTERIUS_TMP_DIR/Setup-simple install --builddir=$ASTERIUS_TMP_DIR/dist/asterius-prelude $ASTERIUS_INSTALL_OPTIONS
cd ..
