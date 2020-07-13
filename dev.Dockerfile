FROM debian:sid-20200607

ARG DEBIAN_FRONTEND=noninteractive

ENV \
  LANG=C.UTF-8 \
  LC_ALL=C.UTF-8 \
  LC_CTYPE=C.UTF-8 \
  PATH=/root/.local/bin:/root/.nvm/versions/node/v14.5.0/bin:${PATH}

RUN \
  echo 'deb [check-valid-until=no] http://snapshot.debian.org/archive/debian/20200713T150707Z sid main contrib non-free' > /etc/apt/sources.list && \
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
    wabt \
    zlib1g-dev \
    zstd && \
  apt autoremove --purge -y && \
  apt clean && \
  rm -rf -v /var/lib/apt/lists/* && \
  cp \
    /etc/skel/.bash_logout \
    /etc/skel/.bashrc \
    /etc/skel/.profile \
    /root

WORKDIR /root

RUN \
  (curl https://raw.githubusercontent.com/nvm-sh/nvm/v0.35.3/install.sh | bash) && \
  bash -c ". ~/.nvm/nvm.sh && nvm install 14.5.0" && \
  echo "eval \"\$(direnv hook bash)\"" >> ~/.bashrc && \
  mkdir -p ~/.local/bin && \
  curl -L https://github.com/commercialhaskell/stack/releases/download/v2.3.1/stack-2.3.1-linux-x86_64-bin -o ~/.local/bin/stack && \
  chmod +x ~/.local/bin/stack && \
  curl -L https://downloads.haskell.org/~cabal/cabal-install-3.2.0.0/cabal-install-3.2.0.0-x86_64-unknown-linux.tar.xz | tar xJ -C ~/.local/bin 'cabal' && \
  npm config set unsafe-perm true && \
  npm install -g \
    0x@4.9.1 \
    parcel-bundler@1.12.4 && \
  pip3 install \
    recommonmark \
    sphinx && \
  mkdir /tmp/asterius

COPY asterius /tmp/asterius/asterius
COPY ghc-toolkit /tmp/asterius/ghc-toolkit
COPY wasm-toolkit /tmp/asterius/wasm-toolkit
COPY stack.yaml /tmp/asterius/stack.yaml

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
  cd /root && \
  rm -rf -v \
    /root/.npm \
    /root/.stack/pantry \
    /root/.stack/programs/*/*.tar.xz \
    /tmp/* \
    /var/tmp/*
