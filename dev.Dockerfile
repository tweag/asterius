FROM debian:sid

ARG DEBIAN_FRONTEND=noninteractive

ENV \
  LANG=C.UTF-8 \
  LC_ALL=C.UTF-8 \
  LC_CTYPE=C.UTF-8 \
  PATH=/home/asterius/.local/bin:${PATH}

RUN \
  echo 'deb [check-valid-until=no] http://snapshot.debian.org/archive/debian/20200322T094224Z sid main contrib non-free' > /etc/apt/sources.list && \
  apt update && \
  apt full-upgrade -y && \
  apt install -y \
    automake \
    cmake \
    curl \
    direnv \
    g++ \
    gawk \
    gcc \
    git \
    gnupg \
    libffi-dev \
    libgmp-dev \
    libncurses-dev \
    libnuma-dev \
    make \
    openssh-client \
    python3-pip \
    sudo \
    xz-utils \
    zlib1g-dev && \
  curl -sSL https://deb.nodesource.com/gpgkey/nodesource.gpg.key | apt-key add - && \
  echo "deb https://deb.nodesource.com/node_13.x sid main" > /etc/apt/sources.list.d/nodesource.list && \
  apt update && \
  apt install -y nodejs && \
  useradd --create-home --shell /bin/bash asterius && \
  echo "asterius ALL=(ALL) NOPASSWD:ALL" >> /etc/sudoers

USER asterius

WORKDIR /home/asterius

RUN \
  echo "eval \"\$(direnv hook bash)\"" >> ~/.bashrc && \
  mkdir -p ~/.local/bin && \
  curl -L https://github.com/commercialhaskell/stack/releases/download/v2.1.3/stack-2.1.3-linux-x86_64.tar.gz | tar xz --wildcards --strip-components=1 -C ~/.local/bin '*/stack' && \
  curl -L https://downloads.haskell.org/~cabal/cabal-install-3.0.0.0/cabal-install-3.0.0.0-x86_64-unknown-linux.tar.xz | tar xJ -C ~/.local/bin 'cabal' && \
  stack update && \
  cabal v1-update && \
  pip3 install \
    recommonmark \
    sphinx && \
  npm config set prefix ~/.local && \
  mkdir /tmp/asterius

COPY --chown=asterius:asterius asterius /tmp/asterius/asterius
COPY --chown=asterius:asterius ghc-toolkit /tmp/asterius/ghc-toolkit
COPY --chown=asterius:asterius npm-utils /tmp/asterius/npm-utils
COPY --chown=asterius:asterius wasm-toolkit /tmp/asterius/wasm-toolkit
COPY --chown=asterius:asterius stack.yaml /tmp/asterius/stack.yaml

RUN \
  cd /tmp/asterius && \
  stack install \
    brittany \
    ghcid \
    hlint \
    ormolu \
    wai-app-static && \
  cd /home/asterius && \
  sudo rm -rf \
    /home/asterius/.stack/programs/*/*.tar.xz \
    /tmp/* \
    /var/lib/apt/lists/* \
    /var/tmp/*
