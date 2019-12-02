#!/bin/sh -e

ahc-cabal v1-update

ahc-cabal v1-install -j --keep-going --minimize-conflict-set --prefix=$ASTERIUS_LIB_DIR --package-db=clear --package-db=global Cabal
ahc-cabal v1-install -j --keep-going --minimize-conflict-set --prefix=$ASTERIUS_LIB_DIR --package-db=clear --package-db=global \
  bibtex \
  binary-tagged \
  blaze-bootstrap \
  blaze-svg \
  blaze-textual \
  cassava-megaparsec \
  cmark \
  cmark-gfm \
  concurrency \
  concurrent-extra \
  concurrent-supply \
  contravariant-extras \
  criterion \
  data-reify \
  distributed-closure \
  doctemplates \
  Earley \
  elerea \
  extrapolate \
  frisby \
  gauge \
  general-games \
  generic-arbitrary \
  generic-data-surgery \
  generic-random \
  genvalidity-aeson \
  genvalidity-bytestring \
  genvalidity-containers \
  genvalidity-hspec-aeson \
  genvalidity-hspec-binary \
  genvalidity-hspec-cereal \
  genvalidity-hspec-hashable \
  genvalidity-hspec-optics \
  genvalidity-path \
  genvalidity-property \
  genvalidity-time \
  genvalidity-uuid \
  graphite \
  gravatar \
  groundhog-th \
  haddock-library \
  HandsomeSoup \
  HaTeX \
  haxl \
  http-client-tls \
  hxt-css \
  hxt-tagsoup \
  integration \
  JuicyPixels-extra \
  lucid-extras \
  matrices \
  megaparsec-tests \
  microlens-aeson \
  microlens-contra \
  persistent-iproute \
  persistent-typed-db \
  pooled-io \
  qrcode-juicypixels \
  quickcheck-arbitrary-adt \
  quickcheck-simple \
  quickcheck-special \
  quickcheck-text \
  quickcheck-transformer \
  quickcheck-unicode \
  regex-applicative-text \
  regex-compat-tdfa \
  relational-schemas \
  rvar \
  safe-money \
  selda-json \
  shake \
  skylighting \
  svg-builder \
  tagchup \
  tardis \
  tasty-expected-failure \
  tasty-hspec \
  tasty-hunit \
  tasty-kat \
  texmath \
  th-desugar \
  th-printf \
  th-utilities \
  tree-diff \
  unicode-transforms \
  unification-fd \
  urlpath \
  vault \
  vector-builder

rm -rf ~/.cabal
