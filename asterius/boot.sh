#!/bin/sh -e

$ASTERIUS_MKDIR_PATH -p $ASTERIUS_BOOT_DIR/package.conf.d
$ASTERIUS_CP_PATH `$ASTERIUS_GHC_PATH --print-libdir`/package.conf.d/rts.conf $ASTERIUS_BOOT_DIR/package.conf.d/
$ASTERIUS_GHCPKG_PATH --package-db=$ASTERIUS_BOOT_DIR/package.conf.d recache
$ASTERIUS_MKDIR_PATH -p $ASTERIUS_TMP_DIR

$ASTERIUS_MKDIR_PATH -p boot-libs
$ASTERIUS_CP_PATH -r $ASTERIUS_BOOT_LIBS_DIR/ghc-prim $ASTERIUS_BOOT_LIBS_DIR/integer-simple $ASTERIUS_BOOT_LIBS_DIR/base boot-libs/
cd boot-libs

$ASTERIUS_GHC_PATH ghc-prim/Setup.hs -no-keep-hi-files -no-keep-o-files -o $ASTERIUS_TMP_DIR/Setup
cd ghc-prim
autoreconf -i
$ASTERIUS_TMP_DIR/Setup configure --builddir=$ASTERIUS_TMP_DIR/dist/ghc-prim --with-ghc=$ASTERIUS_AHC_PATH --prefix=$ASTERIUS_BOOT_DIR --package-db=$ASTERIUS_BOOT_DIR/package.conf.d $ASTERIUS_CONFIGURE_OPTIONS
$ASTERIUS_TMP_DIR/Setup build --builddir=$ASTERIUS_TMP_DIR/dist/ghc-prim $ASTERIUS_BUILD_OPTIONS
$ASTERIUS_TMP_DIR/Setup install --builddir=$ASTERIUS_TMP_DIR/dist/ghc-prim $ASTERIUS_INSTALL_OPTIONS
export ASTERIUS_GHC_PRIM_CONF_PATH=`ls -d $ASTERIUS_BOOT_DIR/package.conf.d/ghc-prim-*.conf`
$ASTERIUS_SED_PATH -i -e 's,^exposed-modules:,exposed-modules: GHC.Prim,' $ASTERIUS_GHC_PRIM_CONF_PATH
$ASTERIUS_GHCPKG_PATH --package-db=$ASTERIUS_BOOT_DIR/package.conf.d recache
cd ..

$ASTERIUS_GHC_PATH integer-simple/Setup.hs -no-keep-hi-files -no-keep-o-files -o $ASTERIUS_TMP_DIR/Setup
cd integer-simple
$ASTERIUS_TMP_DIR/Setup configure --builddir=$ASTERIUS_TMP_DIR/dist/integer-simple --with-ghc=$ASTERIUS_AHC_PATH --prefix=$ASTERIUS_BOOT_DIR --package-db=$ASTERIUS_BOOT_DIR/package.conf.d $ASTERIUS_CONFIGURE_OPTIONS
$ASTERIUS_TMP_DIR/Setup build --builddir=$ASTERIUS_TMP_DIR/dist/integer-simple $ASTERIUS_BUILD_OPTIONS
$ASTERIUS_TMP_DIR/Setup install --builddir=$ASTERIUS_TMP_DIR/dist/integer-simple $ASTERIUS_INSTALL_OPTIONS
cd ..

$ASTERIUS_GHC_PATH base/Setup.hs -no-keep-hi-files -no-keep-o-files -o $ASTERIUS_TMP_DIR/Setup
cd base
autoreconf -i
$ASTERIUS_TMP_DIR/Setup configure --builddir=$ASTERIUS_TMP_DIR/dist/base --with-ghc=$ASTERIUS_AHC_PATH -finteger-simple --prefix=$ASTERIUS_BOOT_DIR --package-db=$ASTERIUS_BOOT_DIR/package.conf.d $ASTERIUS_CONFIGURE_OPTIONS
$ASTERIUS_TMP_DIR/Setup build --builddir=$ASTERIUS_TMP_DIR/dist/base $ASTERIUS_BUILD_OPTIONS
$ASTERIUS_TMP_DIR/Setup install --builddir=$ASTERIUS_TMP_DIR/dist/base $ASTERIUS_INSTALL_OPTIONS
cd ..

cd ..
