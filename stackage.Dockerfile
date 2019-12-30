FROM terrorjack/asterius:base

ARG jobs=1

COPY --chown=asterius:asterius lts.sh /tmp/lts.sh
COPY --chown=asterius:asterius ghc-toolkit/boot-libs/cabal.config /tmp/cabal.config

RUN \
  cd /tmp && \
  ahc-cabal v1-update && \
  mkdir ~/.cabal/bin && \
  ./lts.sh

USER root

RUN \
  rm -rf \
    $ASTERIUS_LIB_DIR/bin \
    /home/asterius/.cabal \
    /tmp/* \
    /var/tmp/*

USER asterius
