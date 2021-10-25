set -xe

MAKE_PACKAGE_PATH=$1

export PATH="$PATH_BZL:$PATH"

export GIT_SSL_CAINFO="/etc/ssl/certs/ca-certificates.crt"

# we assume python3 is in the PATH because we use rules_haskell.
python3 $MAKE_PACKAGE_PATH
