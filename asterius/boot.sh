#!/bin/sh -e

set -x

cp -r $ASTERIUS_BOOT_LIBS_DIR boot-libs
chmod +w -R .
cd boot-libs

$ASTERIUS_GHC ghc-prim/Setup.hs -no-keep-hi-files -no-keep-o-files -threaded -rtsopts -with-rtsopts="-I0 -qg -qb" -o $ASTERIUS_TMP_DIR/Setup-ghc-prim
$ASTERIUS_GHC integer-simple/Setup.hs -no-keep-hi-files -no-keep-o-files -threaded -rtsopts -with-rtsopts="-I0 -qg -qb" -o $ASTERIUS_TMP_DIR/Setup-simple
$ASTERIUS_GHC base/Setup.hs -no-keep-hi-files -no-keep-o-files -threaded -rtsopts -with-rtsopts="-I0 -qg -qb" -o $ASTERIUS_TMP_DIR/Setup-autoconf

cd ghc-prim
$ASTERIUS_TMP_DIR/Setup-ghc-prim configure --prefix=$ASTERIUS_LIB_DIR --package-db=clear --package-db=global --builddir=$ASTERIUS_TMP_DIR/dist/ghc-prim --with-ghc=$ASTERIUS_AHC --with-ghc-pkg=$ASTERIUS_AHCPKG $ASTERIUS_CONFIGURE_OPTIONS
$ASTERIUS_TMP_DIR/Setup-ghc-prim build --builddir=$ASTERIUS_TMP_DIR/dist/ghc-prim $ASTERIUS_BUILD_OPTIONS
$ASTERIUS_TMP_DIR/Setup-ghc-prim install --builddir=$ASTERIUS_TMP_DIR/dist/ghc-prim $ASTERIUS_INSTALL_OPTIONS
cd ..

cd integer-simple
$ASTERIUS_TMP_DIR/Setup-simple configure --prefix=$ASTERIUS_LIB_DIR --package-db=clear --package-db=global --builddir=$ASTERIUS_TMP_DIR/dist/integer-simple --with-ghc=$ASTERIUS_AHC --with-ghc-pkg=$ASTERIUS_AHCPKG --ghc-option=-DASTERIUS $ASTERIUS_CONFIGURE_OPTIONS
$ASTERIUS_TMP_DIR/Setup-simple build --builddir=$ASTERIUS_TMP_DIR/dist/integer-simple $ASTERIUS_BUILD_OPTIONS
$ASTERIUS_TMP_DIR/Setup-simple install --builddir=$ASTERIUS_TMP_DIR/dist/integer-simple $ASTERIUS_INSTALL_OPTIONS
cd ..

cd base
autoreconf -i
$ASTERIUS_TMP_DIR/Setup-autoconf configure --prefix=$ASTERIUS_LIB_DIR --package-db=clear --package-db=global --builddir=$ASTERIUS_TMP_DIR/dist/base --with-ghc=$ASTERIUS_AHC --with-ghc-pkg=$ASTERIUS_AHCPKG -finteger-simple --ghc-option=-DASTERIUS $ASTERIUS_CONFIGURE_OPTIONS
$ASTERIUS_TMP_DIR/Setup-autoconf build --builddir=$ASTERIUS_TMP_DIR/dist/base $ASTERIUS_BUILD_OPTIONS
$ASTERIUS_TMP_DIR/Setup-autoconf install --builddir=$ASTERIUS_TMP_DIR/dist/base $ASTERIUS_INSTALL_OPTIONS
cd ..

cd array
$ASTERIUS_TMP_DIR/Setup-simple configure --prefix=$ASTERIUS_LIB_DIR --package-db=clear --package-db=global --builddir=$ASTERIUS_TMP_DIR/dist/array --with-ghc=$ASTERIUS_AHC --with-ghc-pkg=$ASTERIUS_AHCPKG $ASTERIUS_CONFIGURE_OPTIONS
$ASTERIUS_TMP_DIR/Setup-simple build --builddir=$ASTERIUS_TMP_DIR/dist/array $ASTERIUS_BUILD_OPTIONS
$ASTERIUS_TMP_DIR/Setup-simple install --builddir=$ASTERIUS_TMP_DIR/dist/array $ASTERIUS_INSTALL_OPTIONS
cd ..

cd deepseq
$ASTERIUS_TMP_DIR/Setup-simple configure --prefix=$ASTERIUS_LIB_DIR --package-db=clear --package-db=global --builddir=$ASTERIUS_TMP_DIR/dist/deepseq --with-ghc=$ASTERIUS_AHC --with-ghc-pkg=$ASTERIUS_AHCPKG $ASTERIUS_CONFIGURE_OPTIONS
$ASTERIUS_TMP_DIR/Setup-simple build --builddir=$ASTERIUS_TMP_DIR/dist/deepseq $ASTERIUS_BUILD_OPTIONS
$ASTERIUS_TMP_DIR/Setup-simple install --builddir=$ASTERIUS_TMP_DIR/dist/deepseq $ASTERIUS_INSTALL_OPTIONS
cd ..

cd containers
$ASTERIUS_TMP_DIR/Setup-simple configure --prefix=$ASTERIUS_LIB_DIR --package-db=clear --package-db=global --builddir=$ASTERIUS_TMP_DIR/dist/containers --with-ghc=$ASTERIUS_AHC --with-ghc-pkg=$ASTERIUS_AHCPKG $ASTERIUS_CONFIGURE_OPTIONS
$ASTERIUS_TMP_DIR/Setup-simple build --builddir=$ASTERIUS_TMP_DIR/dist/containers $ASTERIUS_BUILD_OPTIONS
$ASTERIUS_TMP_DIR/Setup-simple install --builddir=$ASTERIUS_TMP_DIR/dist/containers $ASTERIUS_INSTALL_OPTIONS
cd ..

cd transformers
$ASTERIUS_TMP_DIR/Setup-simple configure --prefix=$ASTERIUS_LIB_DIR --package-db=clear --package-db=global --builddir=$ASTERIUS_TMP_DIR/dist/transformers --with-ghc=$ASTERIUS_AHC --with-ghc-pkg=$ASTERIUS_AHCPKG $ASTERIUS_CONFIGURE_OPTIONS
$ASTERIUS_TMP_DIR/Setup-simple build --builddir=$ASTERIUS_TMP_DIR/dist/transformers $ASTERIUS_BUILD_OPTIONS
$ASTERIUS_TMP_DIR/Setup-simple install --builddir=$ASTERIUS_TMP_DIR/dist/transformers $ASTERIUS_INSTALL_OPTIONS
cd ..

cd mtl
$ASTERIUS_TMP_DIR/Setup-simple configure --prefix=$ASTERIUS_LIB_DIR --package-db=clear --package-db=global --builddir=$ASTERIUS_TMP_DIR/dist/mtl --with-ghc=$ASTERIUS_AHC --with-ghc-pkg=$ASTERIUS_AHCPKG $ASTERIUS_CONFIGURE_OPTIONS
$ASTERIUS_TMP_DIR/Setup-simple build --builddir=$ASTERIUS_TMP_DIR/dist/mtl $ASTERIUS_BUILD_OPTIONS
$ASTERIUS_TMP_DIR/Setup-simple install --builddir=$ASTERIUS_TMP_DIR/dist/mtl $ASTERIUS_INSTALL_OPTIONS
cd ..

cd pretty
$ASTERIUS_TMP_DIR/Setup-simple configure --prefix=$ASTERIUS_LIB_DIR --package-db=clear --package-db=global --builddir=$ASTERIUS_TMP_DIR/dist/pretty --with-ghc=$ASTERIUS_AHC --with-ghc-pkg=$ASTERIUS_AHCPKG $ASTERIUS_CONFIGURE_OPTIONS
$ASTERIUS_TMP_DIR/Setup-simple build --builddir=$ASTERIUS_TMP_DIR/dist/pretty $ASTERIUS_BUILD_OPTIONS
$ASTERIUS_TMP_DIR/Setup-simple install --builddir=$ASTERIUS_TMP_DIR/dist/pretty $ASTERIUS_INSTALL_OPTIONS
cd ..

cd ghc-boot-th
$ASTERIUS_TMP_DIR/Setup-simple configure --prefix=$ASTERIUS_LIB_DIR --package-db=clear --package-db=global --builddir=$ASTERIUS_TMP_DIR/dist/ghc-boot-th --with-ghc=$ASTERIUS_AHC --with-ghc-pkg=$ASTERIUS_AHCPKG $ASTERIUS_CONFIGURE_OPTIONS
$ASTERIUS_TMP_DIR/Setup-simple build --builddir=$ASTERIUS_TMP_DIR/dist/ghc-boot-th $ASTERIUS_BUILD_OPTIONS
$ASTERIUS_TMP_DIR/Setup-simple install --builddir=$ASTERIUS_TMP_DIR/dist/ghc-boot-th $ASTERIUS_INSTALL_OPTIONS
cd ..

cd template-haskell
$ASTERIUS_TMP_DIR/Setup-simple configure --prefix=$ASTERIUS_LIB_DIR --package-db=clear --package-db=global --builddir=$ASTERIUS_TMP_DIR/dist/template-haskell --with-ghc=$ASTERIUS_AHC --with-ghc-pkg=$ASTERIUS_AHCPKG $ASTERIUS_CONFIGURE_OPTIONS
$ASTERIUS_TMP_DIR/Setup-simple build --builddir=$ASTERIUS_TMP_DIR/dist/template-haskell $ASTERIUS_BUILD_OPTIONS
$ASTERIUS_TMP_DIR/Setup-simple install --builddir=$ASTERIUS_TMP_DIR/dist/template-haskell $ASTERIUS_INSTALL_OPTIONS
cd ..

cd bytestring
$ASTERIUS_TMP_DIR/Setup-simple configure --prefix=$ASTERIUS_LIB_DIR --package-db=clear --package-db=global --builddir=$ASTERIUS_TMP_DIR/dist/bytestring --with-ghc=$ASTERIUS_AHC --with-ghc-pkg=$ASTERIUS_AHCPKG -finteger-simple --ghc-option=-DASTERIUS $ASTERIUS_CONFIGURE_OPTIONS
$ASTERIUS_TMP_DIR/Setup-simple build --builddir=$ASTERIUS_TMP_DIR/dist/bytestring $ASTERIUS_BUILD_OPTIONS
$ASTERIUS_TMP_DIR/Setup-simple install --builddir=$ASTERIUS_TMP_DIR/dist/bytestring $ASTERIUS_INSTALL_OPTIONS
cd ..

cd binary
$ASTERIUS_TMP_DIR/Setup-simple configure --prefix=$ASTERIUS_LIB_DIR --package-db=clear --package-db=global --builddir=$ASTERIUS_TMP_DIR/dist/binary --with-ghc=$ASTERIUS_AHC --with-ghc-pkg=$ASTERIUS_AHCPKG $ASTERIUS_CONFIGURE_OPTIONS
$ASTERIUS_TMP_DIR/Setup-simple build --builddir=$ASTERIUS_TMP_DIR/dist/binary $ASTERIUS_BUILD_OPTIONS
$ASTERIUS_TMP_DIR/Setup-simple install --builddir=$ASTERIUS_TMP_DIR/dist/binary $ASTERIUS_INSTALL_OPTIONS
cd ..

cd xhtml
$ASTERIUS_TMP_DIR/Setup-simple configure --prefix=$ASTERIUS_LIB_DIR --package-db=clear --package-db=global --builddir=$ASTERIUS_TMP_DIR/dist/xhtml --with-ghc=$ASTERIUS_AHC --with-ghc-pkg=$ASTERIUS_AHCPKG $ASTERIUS_CONFIGURE_OPTIONS
$ASTERIUS_TMP_DIR/Setup-simple build --builddir=$ASTERIUS_TMP_DIR/dist/xhtml $ASTERIUS_BUILD_OPTIONS
$ASTERIUS_TMP_DIR/Setup-simple install --builddir=$ASTERIUS_TMP_DIR/dist/xhtml $ASTERIUS_INSTALL_OPTIONS
cd ..

cd filepath
$ASTERIUS_TMP_DIR/Setup-simple configure --prefix=$ASTERIUS_LIB_DIR --package-db=clear --package-db=global --builddir=$ASTERIUS_TMP_DIR/dist/filepath --with-ghc=$ASTERIUS_AHC --with-ghc-pkg=$ASTERIUS_AHCPKG $ASTERIUS_CONFIGURE_OPTIONS
$ASTERIUS_TMP_DIR/Setup-simple build --builddir=$ASTERIUS_TMP_DIR/dist/filepath $ASTERIUS_BUILD_OPTIONS
$ASTERIUS_TMP_DIR/Setup-simple install --builddir=$ASTERIUS_TMP_DIR/dist/filepath $ASTERIUS_INSTALL_OPTIONS
cd ..

cd time
autoreconf -i
$ASTERIUS_TMP_DIR/Setup-autoconf configure --prefix=$ASTERIUS_LIB_DIR --package-db=clear --package-db=global --builddir=$ASTERIUS_TMP_DIR/dist/time --with-ghc=$ASTERIUS_AHC --with-ghc-pkg=$ASTERIUS_AHCPKG $ASTERIUS_CONFIGURE_OPTIONS
$ASTERIUS_TMP_DIR/Setup-autoconf build --builddir=$ASTERIUS_TMP_DIR/dist/time $ASTERIUS_BUILD_OPTIONS
$ASTERIUS_TMP_DIR/Setup-autoconf install --builddir=$ASTERIUS_TMP_DIR/dist/time $ASTERIUS_INSTALL_OPTIONS
cd ..

cd unix
autoreconf -i
$ASTERIUS_TMP_DIR/Setup-autoconf configure --prefix=$ASTERIUS_LIB_DIR --package-db=clear --package-db=global --builddir=$ASTERIUS_TMP_DIR/dist/unix --with-ghc=$ASTERIUS_AHC --with-ghc-pkg=$ASTERIUS_AHCPKG --hsc2hs-option=-DASTERIUS $ASTERIUS_CONFIGURE_OPTIONS
$ASTERIUS_TMP_DIR/Setup-autoconf build --builddir=$ASTERIUS_TMP_DIR/dist/unix $ASTERIUS_BUILD_OPTIONS
$ASTERIUS_TMP_DIR/Setup-autoconf install --builddir=$ASTERIUS_TMP_DIR/dist/unix $ASTERIUS_INSTALL_OPTIONS
cd ..

cd directory
autoreconf -i
$ASTERIUS_TMP_DIR/Setup-autoconf configure --prefix=$ASTERIUS_LIB_DIR --package-db=clear --package-db=global --builddir=$ASTERIUS_TMP_DIR/dist/directory --with-ghc=$ASTERIUS_AHC --with-ghc-pkg=$ASTERIUS_AHCPKG $ASTERIUS_CONFIGURE_OPTIONS
$ASTERIUS_TMP_DIR/Setup-autoconf build --builddir=$ASTERIUS_TMP_DIR/dist/directory $ASTERIUS_BUILD_OPTIONS
$ASTERIUS_TMP_DIR/Setup-autoconf install --builddir=$ASTERIUS_TMP_DIR/dist/directory $ASTERIUS_INSTALL_OPTIONS
cd ..

cd ghc-boot
$ASTERIUS_TMP_DIR/Setup-simple configure --prefix=$ASTERIUS_LIB_DIR --package-db=clear --package-db=global --builddir=$ASTERIUS_TMP_DIR/dist/ghc-boot --with-ghc=$ASTERIUS_AHC --with-ghc-pkg=$ASTERIUS_AHCPKG $ASTERIUS_CONFIGURE_OPTIONS
$ASTERIUS_TMP_DIR/Setup-simple build --builddir=$ASTERIUS_TMP_DIR/dist/ghc-boot $ASTERIUS_BUILD_OPTIONS
$ASTERIUS_TMP_DIR/Setup-simple install --builddir=$ASTERIUS_TMP_DIR/dist/ghc-boot $ASTERIUS_INSTALL_OPTIONS
cd ..

cd ghc-heap
$ASTERIUS_TMP_DIR/Setup-simple configure --prefix=$ASTERIUS_LIB_DIR --package-db=clear --package-db=global --builddir=$ASTERIUS_TMP_DIR/dist/ghc-heap --with-ghc=$ASTERIUS_AHC --with-ghc-pkg=$ASTERIUS_AHCPKG $ASTERIUS_CONFIGURE_OPTIONS
$ASTERIUS_TMP_DIR/Setup-simple build --builddir=$ASTERIUS_TMP_DIR/dist/ghc-heap $ASTERIUS_BUILD_OPTIONS
$ASTERIUS_TMP_DIR/Setup-simple install --builddir=$ASTERIUS_TMP_DIR/dist/ghc-heap $ASTERIUS_INSTALL_OPTIONS
cd ..

# cd ghci
# $ASTERIUS_TMP_DIR/Setup-simple configure --prefix=$ASTERIUS_LIB_DIR --package-db=clear --package-db=global --builddir=$ASTERIUS_TMP_DIR/dist/ghci --with-ghc=$ASTERIUS_AHC --with-ghc-pkg=$ASTERIUS_AHCPKG -fghci $ASTERIUS_CONFIGURE_OPTIONS
# $ASTERIUS_TMP_DIR/Setup-simple build --builddir=$ASTERIUS_TMP_DIR/dist/ghci $ASTERIUS_BUILD_OPTIONS
# $ASTERIUS_TMP_DIR/Setup-simple install --builddir=$ASTERIUS_TMP_DIR/dist/ghci $ASTERIUS_INSTALL_OPTIONS
# cd ..

cd ..
