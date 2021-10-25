set -e

OUTPUT_DATADIR="$1"

# setup working directory for ahc-boot
export ASTERIUS_DATA_DIR="$(pwd)/asterius"
export asterius_datadir=$ASTERIUS_DATA_DIR

cp -r ghc-toolkit/ghc-libdir $ASTERIUS_DATA_DIR
cp -r ghc-toolkit/boot-libs $ASTERIUS_DATA_DIR


# Put dependencies in the PATH
for p in $PATH_BZL; do
    PATH="$(readlink -f $p):$PATH"
done
	 
ahc-boot

cp -a "$ASTERIUS_DATA_DIR/." "$OUTPUT_DATADIR/"

