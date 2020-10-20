FROM debian:sid-slim

ARG DEBIAN_FRONTEND=noninteractive

ENV \
  BROWSER=echo \
  LANG=C.UTF-8 \
  LC_ALL=C.UTF-8 \
  LC_CTYPE=C.UTF-8 \
  PATH=/root/.local/bin:/root/.nvm/versions/node/v15.0.0/bin:${PATH} \
  WASI_SDK_PATH=/opt/wasi-sdk

RUN \
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
    libtinfo5 \
    openssh-client \
    python3-pip \
    ripgrep \
    wabt \
    xdg-utils \
    zlib1g-dev \
    zstd && \
  mkdir -p ${WASI_SDK_PATH} && \
  (curl -L https://github.com/TerrorJack/wasi-sdk/releases/download/201014/wasi-sdk-11.5g3cbd9d212e9a-linux.tar.gz | tar xz -C ${WASI_SDK_PATH} --strip-components=1) && \
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
  (curl https://raw.githubusercontent.com/nvm-sh/nvm/v0.36.0/install.sh | bash) && \
  bash -c ". ~/.nvm/nvm.sh && nvm install 15.0.0" && \
  echo "eval \"\$(direnv hook bash)\"" >> ~/.bashrc && \
  mkdir -p ~/.local/bin && \
  curl -L https://github.com/commercialhaskell/stack/releases/download/v2.5.1/stack-2.5.1-linux-x86_64-bin -o ~/.local/bin/stack && \
  chmod +x ~/.local/bin/stack && \
  curl -L https://downloads.haskell.org/~cabal/cabal-install-3.2.0.0/cabal-install-3.2.0.0-x86_64-unknown-linux.tar.xz | tar xJ -C ~/.local/bin 'cabal' && \
  npm config set unsafe-perm true && \
  npm install -g \
    @cloudflare/wrangler \
    0x \
    webpack \
    webpack-cli && \
  pip3 install \
    recommonmark \
    sphinx && \
  mkdir /tmp/asterius

COPY . /tmp/asterius

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
