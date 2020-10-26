FROM debian:sid-slim

ARG DEBIAN_FRONTEND=noninteractive

ARG NODE_VER=15.0.1

ENV \
  BROWSER=echo \
  LANG=C.UTF-8 \
  LC_ALL=C.UTF-8 \
  LC_CTYPE=C.UTF-8 \
  PATH=/root/.local/bin:/root/.nvm/versions/node/v${NODE_VER}/bin:${PATH} \
  WASI_SDK_PATH=/opt/wasi-sdk

RUN \
  apt update && \
  apt full-upgrade -y && \
  apt install -y \
    alex \
    automake \
    binaryen \
    build-essential \
    c2hs \
    cpphs \
    curl \
    direnv \
    gawk \
    git \
    happy \
    hlint \
    libffi-dev \
    libgmp-dev \
    libncurses-dev \
    openssh-client \
    python3-pip \
    ripgrep \
    wabt \
    xdg-utils \
    zlib1g-dev \
    zstd && \
  mkdir -p ${WASI_SDK_PATH} && \
  (curl -L https://github.com/TerrorJack/wasi-sdk/releases/download/201026/wasi-sdk-11.6gfada4f778bbb-linux.tar.gz | tar xz -C ${WASI_SDK_PATH} --strip-components=1) && \
  apt autoremove --purge -y && \
  apt clean && \
  rm -rf -v /var/lib/apt/lists/* && \
  cp \
    /etc/skel/.bash_logout \
    /etc/skel/.bashrc \
    /etc/skel/.profile \
    /root

WORKDIR /root

COPY . /tmp/asterius

RUN \
  echo "eval \"\$(direnv hook bash)\"" >> ~/.bashrc && \
  (curl https://raw.githubusercontent.com/nvm-sh/nvm/v0.36.0/install.sh | bash) && \
  bash -i -c "nvm install ${NODE_VER}" && \
  patch ~/.nvm/versions/node/v${NODE_VER}/lib/node_modules/npm/node_modules/@npmcli/promise-spawn/index.js /tmp/asterius/utils/promise-spawn.patch && \
  bash -i -c "npm install -g --unsafe-perm=true --allow-root @cloudflare/wrangler webpack webpack-cli" && \
  mkdir -p ~/.local/bin && \
  curl -L https://github.com/commercialhaskell/stack/releases/download/v2.5.1/stack-2.5.1-linux-x86_64-bin -o ~/.local/bin/stack && \
  chmod +x ~/.local/bin/stack && \
  curl -L https://downloads.haskell.org/~cabal/cabal-install-3.2.0.0/cabal-install-3.2.0.0-x86_64-unknown-linux.tar.xz | tar xJ -C ~/.local/bin 'cabal' && \
  pip3 install \
    recommonmark \
    sphinx

RUN \
  cd /tmp/asterius && \
  stack --no-terminal update && \
  stack --no-terminal install \
    brittany \
    ghcid \
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
