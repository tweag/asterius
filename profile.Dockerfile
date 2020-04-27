FROM debian:sid

ARG DEBIAN_FRONTEND=noninteractive

ARG jobs=1

ARG ASTERIUS_AHC_LD_IGNORE=1

ARG UID=1000

ARG GID=1000

ENV \
  ASTERIUS_LIB_DIR=/home/asterius/.asterius-local-install-root/share/x86_64-linux-ghc-8.8.3/asterius-0.0.1/.boot/asterius_lib \
  LANG=C.UTF-8 \
  LC_ALL=C.UTF-8 \
  LC_CTYPE=C.UTF-8 \
  PATH=/home/asterius/.asterius-local-install-root/bin:/home/asterius/.asterius-snapshot-install-root/bin:/home/asterius/.asterius-compiler-bin:/home/asterius/.local/bin:/home/asterius/.nvm/bin:${PATH}

RUN \
  echo 'deb [check-valid-until=no] http://snapshot.debian.org/archive/debian/20200426T030336Z sid main contrib non-free' > /etc/apt/sources.list && \
  apt update && \
  apt full-upgrade -y && \
  apt install -y \
    automake \
    binaryen \
    curl \
    gawk \
    gcc \
    git \
    libffi-dev \
    libgmp-dev \
    libncurses-dev \
    libnuma-dev \
    libstdc++-9-dev \
    make \
    sudo \
    xz-utils \
    zlib1g-dev && \
  groupadd --gid $GID asterius && \
  useradd \
    --create-home \
    --shell /bin/bash \
    --uid $UID \
    --gid $GID \
    asterius && \
  echo "asterius ALL=(ALL) NOPASSWD:ALL" >> /etc/sudoers

USER asterius

WORKDIR /home/asterius

RUN \
  (curl https://raw.githubusercontent.com/nvm-sh/nvm/v0.35.3/install.sh | bash) && \
  bash -c ". ~/.nvm/nvm.sh && nvm install 14.0.0 && ln -s \$NVM_BIN ~/.nvm/bin" && \
  mkdir -p ~/.local/bin && \
  curl -L https://github.com/commercialhaskell/stack/releases/download/v2.3.0.1/stack-2.3.0.1-linux-x86_64.tar.gz | tar xz --wildcards --strip-components=1 -C ~/.local/bin '*/stack' && \
  curl -L https://downloads.haskell.org/~cabal/cabal-install-3.2.0.0/cabal-install-3.2.0.0-x86_64-unknown-linux.tar.xz | tar xJ -C ~/.local/bin 'cabal' && \
  mkdir ~/.asterius

COPY --chown=asterius:asterius asterius /home/asterius/.asterius/asterius
COPY --chown=asterius:asterius ghc-toolkit /home/asterius/.asterius/ghc-toolkit
COPY --chown=asterius:asterius npm-utils /home/asterius/.asterius/npm-utils
COPY --chown=asterius:asterius wasm-toolkit /home/asterius/.asterius/wasm-toolkit
COPY --chown=asterius:asterius lts-profile.sh /home/asterius/.asterius/lts.sh
COPY --chown=asterius:asterius stack-profile.yaml /home/asterius/.asterius/stack.yaml

RUN \
  cd ~/.asterius && \
  stack --no-terminal update && \
  stack --no-terminal build \
    asterius \
    alex \
    happy \
    c2hs \
    cpphs && \
  ln -s $(stack path --local-install-root) ~/.asterius-local-install-root && \
  ln -s $(stack path --snapshot-install-root) ~/.asterius-snapshot-install-root && \
  ln -s $(stack path --compiler-bin) ~/.asterius-compiler-bin && \
  ahc-boot && \
  mkdir ~/.cabal/bin && \
  ln -s ghc-toolkit/boot-libs/cabal.config cabal.config && \
  mv ~/.nvm/bin/node ~/.nvm/bin/node.bak && \
  (./lts.sh || true) && \
  mv ~/.nvm/bin/node.bak ~/.nvm/bin/node
