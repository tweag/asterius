FROM debian:sid

ARG DEBIAN_FRONTEND=noninteractive

ENV \
  LANG=C.UTF-8 \
  LC_ALL=C.UTF-8 \
  LC_CTYPE=C.UTF-8 \
  PATH=/home/asterius/.local/bin:/home/asterius/.nvm/versions/node/v14.2.0/bin:${PATH}

RUN \
  echo 'deb [check-valid-until=no] http://snapshot.debian.org/archive/debian/20200503T025701Z sid main contrib non-free' > /etc/apt/sources.list && \
  apt update && \
  apt full-upgrade -y && \
  apt install -y \
    automake \
    binaryen \
    build-essential \
    curl \
    direnv \
    gawk \
    git \
    libffi-dev \
    libgmp-dev \
    libncurses-dev \
    libnuma-dev \
    openssh-client \
    python3-pip \
    ripgrep \
    sudo \
    wabt \
    zlib1g-dev \
    zstd && \
  apt autoremove --purge -y && \
  apt clean && \
  rm -rf -v /var/lib/apt/lists/* && \
  useradd --create-home --shell /bin/bash asterius && \
  echo "asterius ALL=(ALL) NOPASSWD:ALL" >> /etc/sudoers

USER asterius

WORKDIR /home/asterius

RUN \
  (curl https://raw.githubusercontent.com/nvm-sh/nvm/v0.35.3/install.sh | bash) && \
  sh -c ". ~/.nvm/nvm.sh && nvm install 14.2.0" && \
  echo "eval \"\$(direnv hook bash)\"" >> ~/.bashrc && \
  mkdir -p ~/.local/bin && \
  curl -L https://github.com/commercialhaskell/stack/releases/download/v2.3.1/stack-2.3.1-linux-x86_64-bin -o ~/.local/bin/stack && \
  chmod +x ~/.local/bin/stack && \
  curl -L https://downloads.haskell.org/~cabal/cabal-install-3.2.0.0/cabal-install-3.2.0.0-x86_64-unknown-linux.tar.xz | tar xJ -C ~/.local/bin 'cabal' && \
  pip3 install \
    recommonmark \
    sphinx && \
  mkdir /tmp/asterius

COPY --chown=asterius:asterius asterius /tmp/asterius/asterius
COPY --chown=asterius:asterius ghc-toolkit /tmp/asterius/ghc-toolkit
COPY --chown=asterius:asterius npm-utils /tmp/asterius/npm-utils
COPY --chown=asterius:asterius wasm-toolkit /tmp/asterius/wasm-toolkit
COPY --chown=asterius:asterius stack.yaml /tmp/asterius/stack.yaml

RUN \
  cd /tmp/asterius && \
  stack --no-terminal update && \
  stack --no-terminal install \
    alex \
    brittany \
    c2hs \
    cpphs \
    ghcid \
    happy \
    hlint \
    ormolu \
    pretty-show \
    wai-app-static && \
  cd /home/asterius && \
  sudo rm -rf -v \
    /home/asterius/.stack/pantry \
    /home/asterius/.stack/programs/*/*.tar.xz \
    /tmp/* \
    /var/tmp/*
