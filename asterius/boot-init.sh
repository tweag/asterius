#!/bin/sh

set -eu

rm -rf $AHC_TMPDIR $AHC_LIBDIR
mkdir -p $AHC_TMPDIR
mkdir -p $AHC_LIBDIR
cp -r $ASTERIUS_SANDBOX_GHC_LIBDIR/. $AHC_LIBDIR
mkdir $AHC_LIBDIR/package.conf.d
cp $ASTERIUS_BOOT_LIBS_DIR/rts/rts.conf $AHC_LIBDIR/package.conf.d/
ahc-pkg --package-db=$AHC_LIBDIR/package.conf.d recache
mkdir $AHC_LIBDIR/rts
