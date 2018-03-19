#!/bin/sh -e

$ASTERIUS_MKDIR_PATH -p $ASTERIUS_TMP_DIR

$ASTERIUS_MKDIR_PATH -p boot-libs
$ASTERIUS_CP_PATH -r $ASTERIUS_BOOT_LIBS_DIR/ghc-prim $ASTERIUS_BOOT_LIBS_DIR/integer-simple $ASTERIUS_BOOT_LIBS_DIR/base boot-libs/
cd boot-libs

$ASTERIUS_GHC_PATH ghc-prim/Setup.hs -no-keep-hi-files -no-keep-o-files -o $ASTERIUS_TMP_DIR/Setup
cd ghc-prim
autoreconf -i
$ASTERIUS_TMP_DIR/Setup configure --builddir=$ASTERIUS_TMP_DIR/dist/ghc-prim --with-ghc=$ASTERIUS_AHC_PATH $ASTERIUS_CONFIGURE_OPTIONS
$ASTERIUS_TMP_DIR/Setup build --builddir=$ASTERIUS_TMP_DIR/dist/ghc-prim $ASTERIUS_BUILD_OPTIONS
cd ..

$ASTERIUS_GHC_PATH integer-simple/Setup.hs -no-keep-hi-files -no-keep-o-files -o $ASTERIUS_TMP_DIR/Setup
cd integer-simple
$ASTERIUS_TMP_DIR/Setup configure --builddir=$ASTERIUS_TMP_DIR/dist/integer-simple --with-ghc=$ASTERIUS_AHC_PATH $ASTERIUS_CONFIGURE_OPTIONS
$ASTERIUS_TMP_DIR/Setup build --builddir=$ASTERIUS_TMP_DIR/dist/integer-simple $ASTERIUS_BUILD_OPTIONS
cd ..

$ASTERIUS_GHC_PATH base/Setup.hs -no-keep-hi-files -no-keep-o-files -o $ASTERIUS_TMP_DIR/Setup
cd base
autoreconf -i
$ASTERIUS_TMP_DIR/Setup configure --builddir=$ASTERIUS_TMP_DIR/dist/base --with-ghc=$ASTERIUS_AHC_PATH -finteger-simple $ASTERIUS_CONFIGURE_OPTIONS
$ASTERIUS_TMP_DIR/Setup build --builddir=$ASTERIUS_TMP_DIR/dist/base $ASTERIUS_BUILD_OPTIONS
cd ..

cd ..
