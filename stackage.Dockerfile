FROM terrorjack/asterius:base

ARG jobs=1

ARG jobs_th=1

ARG ASTERIUS_AHC_LD_IGNORE=1

COPY ghc-toolkit/boot-libs/cabal.config /tmp/cabal.config
COPY lts.sh /tmp/lts.sh
COPY pkgs.txt /tmp/pkgs.txt

RUN \
  cd /tmp && \
  ahc-cabal v1-update && \
  ./lts.sh

RUN \
  rm -rf \
    $ASTERIUS_LIB_DIR/bin \
    /root/.ahc-cabal \
    /tmp/* \
    /var/tmp/*
