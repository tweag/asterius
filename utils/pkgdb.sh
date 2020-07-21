export ASTERIUS_LIB_DIR=$(stack path --local-install-root)/share/x86_64-linux-ghc-8.8.4/asterius-0.0.1/.boot/asterius_lib
alias ahc-install="ahc-cabal v1-install --prefix=$ASTERIUS_LIB_DIR --package-db=clear --package-db=global"
