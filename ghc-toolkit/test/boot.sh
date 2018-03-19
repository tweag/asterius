#!/bin/sh -e

mkdir -p /tmp/package.conf.d
cp `ghc --print-libdir`/package.conf.d/rts.conf /tmp/package.conf.d/
ghc-pkg --package-db=/tmp/package.conf.d recache

cd ghc-toolkit/boot-libs

ghc ghc-prim/Setup.hs -no-keep-hi-files -no-keep-o-files -o /tmp/Setup
cd ghc-prim
autoreconf -i
/tmp/Setup configure --builddir=/tmp/dist/ghc-prim --prefix=/tmp --package-db=/tmp/package.conf.d
/tmp/Setup build --builddir=/tmp/dist/ghc-prim
/tmp/Setup install --builddir=/tmp/dist/ghc-prim
sed -i -e 's,^exposed-modules:,exposed-modules: GHC.Prim,' `ls -d /tmp/package.conf.d/ghc-prim-*.conf`
ghc-pkg --package-db=/tmp/package.conf.d recache
cd ..

ghc integer-gmp/Setup.hs -no-keep-hi-files -no-keep-o-files -o /tmp/Setup
cd integer-gmp
autoreconf -i
/tmp/Setup configure --builddir=/tmp/dist/integer-gmp --prefix=/tmp --package-db=/tmp/package.conf.d
/tmp/Setup build --builddir=/tmp/dist/integer-gmp
/tmp/Setup install --builddir=/tmp/dist/integer-gmp
cd ..

ghc base/Setup.hs -no-keep-hi-files -no-keep-o-files -o /tmp/Setup
cd base
autoreconf -i
/tmp/Setup configure --builddir=/tmp/dist/base -finteger-gmp --prefix=/tmp --package-db=/tmp/package.conf.d
/tmp/Setup build --builddir=/tmp/dist/base
/tmp/Setup install --builddir=/tmp/dist/base
cd ..

cd ..
