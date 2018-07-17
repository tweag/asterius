#!/bin/sh -e

mkdir -p $ASTERIUS_TMP_DIR
mkdir -p $ASTERIUS_LIB_DIR/package.conf.d
$ASTERIUS_GHCPKG --package-db=$ASTERIUS_LIB_DIR/package.conf.d recache

cp -r $ASTERIUS_BOOT_LIBS_DIR .
cd boot-libs

$ASTERIUS_GHC ghc-prim/Setup.hs -no-keep-hi-files -no-keep-o-files -o $ASTERIUS_TMP_DIR/Setup
cd ghc-prim
autoreconf -i
$ASTERIUS_TMP_DIR/Setup configure --prefix=$ASTERIUS_LIB_DIR --package-db=clear --package-db=global --package-db=$ASTERIUS_LIB_DIR/package.conf.d --builddir=$ASTERIUS_TMP_DIR/dist/ghc-prim --with-ghc=$ASTERIUS_AHC $ASTERIUS_CONFIGURE_OPTIONS
$ASTERIUS_TMP_DIR/Setup build --builddir=$ASTERIUS_TMP_DIR/dist/ghc-prim $ASTERIUS_BUILD_OPTIONS
$ASTERIUS_TMP_DIR/Setup install --builddir=$ASTERIUS_TMP_DIR/dist/ghc-prim $ASTERIUS_INSTALL_OPTIONS
cd ..

$ASTERIUS_GHC integer-simple/Setup.hs -no-keep-hi-files -no-keep-o-files -o $ASTERIUS_TMP_DIR/Setup
cd integer-simple
$ASTERIUS_TMP_DIR/Setup configure --prefix=$ASTERIUS_LIB_DIR --package-db=clear --package-db=global --package-db=$ASTERIUS_LIB_DIR/package.conf.d --builddir=$ASTERIUS_TMP_DIR/dist/integer-simple --with-ghc=$ASTERIUS_AHC $ASTERIUS_CONFIGURE_OPTIONS
$ASTERIUS_TMP_DIR/Setup build --builddir=$ASTERIUS_TMP_DIR/dist/integer-simple $ASTERIUS_BUILD_OPTIONS
$ASTERIUS_TMP_DIR/Setup install --builddir=$ASTERIUS_TMP_DIR/dist/integer-simple $ASTERIUS_INSTALL_OPTIONS
cd ..

$ASTERIUS_GHC base/Setup.hs -no-keep-hi-files -no-keep-o-files -o $ASTERIUS_TMP_DIR/Setup
cd base
autoreconf -i
$ASTERIUS_TMP_DIR/Setup configure --prefix=$ASTERIUS_LIB_DIR --package-db=clear --package-db=global --package-db=$ASTERIUS_LIB_DIR/package.conf.d --builddir=$ASTERIUS_TMP_DIR/dist/base --with-ghc=$ASTERIUS_AHC -finteger-simple $ASTERIUS_CONFIGURE_OPTIONS
$ASTERIUS_TMP_DIR/Setup build --builddir=$ASTERIUS_TMP_DIR/dist/base $ASTERIUS_BUILD_OPTIONS
$ASTERIUS_TMP_DIR/Setup install --builddir=$ASTERIUS_TMP_DIR/dist/base $ASTERIUS_INSTALL_OPTIONS
cd ..

$ASTERIUS_GHC array/Setup.hs -no-keep-hi-files -no-keep-o-files -o $ASTERIUS_TMP_DIR/Setup
cd array
$ASTERIUS_TMP_DIR/Setup configure --prefix=$ASTERIUS_LIB_DIR --package-db=clear --package-db=global --package-db=$ASTERIUS_LIB_DIR/package.conf.d --builddir=$ASTERIUS_TMP_DIR/dist/array --with-ghc=$ASTERIUS_AHC $ASTERIUS_CONFIGURE_OPTIONS
$ASTERIUS_TMP_DIR/Setup build --builddir=$ASTERIUS_TMP_DIR/dist/array $ASTERIUS_BUILD_OPTIONS
$ASTERIUS_TMP_DIR/Setup install --builddir=$ASTERIUS_TMP_DIR/dist/array $ASTERIUS_INSTALL_OPTIONS
cd ..

cd ..
