FROM terrorjack/asterius:base

ARG jobs=1

ARG jobs_th=1

ARG ASTERIUS_AHC_LD_IGNORE=1

COPY lts.sh /tmp/lts.sh
COPY ghc-toolkit/boot-libs/cabal.config /tmp/cabal.config

RUN \
  cd /tmp && \
  ahc-cabal v1-update && \
  ./lts.sh

RUN \
  rm -rf -v \
    $ASTERIUS_LIB_DIR/bin \
    /root/.cabal \
    /tmp/* \
    /var/tmp/*
