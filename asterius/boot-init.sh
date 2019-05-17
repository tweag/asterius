#!/bin/sh -e

set -x

rm -rf $ASTERIUS_TMP_DIR $ASTERIUS_LIB_DIR
mkdir -p $ASTERIUS_TMP_DIR
mkdir -p $ASTERIUS_LIB_DIR
cp -r $ASTERIUS_SANDBOX_GHC_LIBDIR/include \
      $ASTERIUS_SANDBOX_GHC_LIBDIR/llvm-passes \
      $ASTERIUS_SANDBOX_GHC_LIBDIR/llvm-targets \
      $ASTERIUS_SANDBOX_GHC_LIBDIR/platformConstants \
      $ASTERIUS_SANDBOX_GHC_LIBDIR/settings \
      $ASTERIUS_SANDBOX_GHC_LIBDIR/template-hsc.h \
      $ASTERIUS_LIB_DIR
mkdir $ASTERIUS_LIB_DIR/package.conf.d
cp $ASTERIUS_BOOT_LIBS_DIR/rts/rts.conf $ASTERIUS_LIB_DIR/package.conf.d/
$ASTERIUS_AHCPKG --package-db=$ASTERIUS_LIB_DIR/package.conf.d recache
mkdir $ASTERIUS_LIB_DIR/rts
