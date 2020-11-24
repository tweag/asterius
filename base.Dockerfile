FROM debian:sid-slim

ARG DEBIAN_FRONTEND=noninteractive

ARG NODE_VER=15.3.0

ENV \
  LANG=C.UTF-8 \
  PATH=/root/.asterius-local-install-root/bin:/root/.asterius-snapshot-install-root/bin:/root/.asterius-compiler-bin:/root/.local/bin:/root/.nvm/versions/node/v${NODE_VER}/bin:${PATH} \
  WASI_SDK_PATH=/opt/wasi-sdk

RUN \
  apt update && \
  apt full-upgrade -y && \
  apt install -y \
    alex \
    automake \
    build-essential \
    binaryen \
    c2hs \
    cpphs \
    curl \
    gawk \
    git \
    happy \
    libffi-dev \
    libgmp-dev \
    libncurses-dev \
    python3-minimal \
    zlib1g-dev && \
  mkdir -p ${WASI_SDK_PATH} && \
  (curl -L https://github.com/TerrorJack/wasi-sdk/releases/download/201027/wasi-sdk-11.6gc1fd249a52ea-linux.tar.gz | tar xz -C ${WASI_SDK_PATH} --strip-components=1) && \
  cp \
    /etc/skel/.bash_logout \
    /etc/skel/.bashrc \
    /etc/skel/.profile \
    /root

WORKDIR /root

COPY . /root/.asterius

RUN \
  (curl https://raw.githubusercontent.com/nvm-sh/nvm/v0.37.0/install.sh | bash) && \
  bash -i -c "nvm install ${NODE_VER}" && \
  patch ~/.nvm/versions/node/v${NODE_VER}/lib/node_modules/npm/node_modules/@npmcli/promise-spawn/index.js ~/.asterius/utils/promise-spawn.patch && \
  bash -i -c "npm install -g --unsafe-perm=true --allow-root @cloudflare/wrangler webpack webpack-cli" && \
  mkdir -p ~/.local/bin && \
  curl -L https://github.com/commercialhaskell/stack/releases/download/v2.5.1/stack-2.5.1-linux-x86_64-bin -o ~/.local/bin/stack && \
  chmod +x ~/.local/bin/stack && \
  curl -L https://oleg.fi/cabal-install-3.4.0.0-rc4/cabal-install-3.4.0.0-x86_64-ubuntu-16.04.tar.xz | tar xJ -C ~/.local/bin --wildcards '*/cabal'

RUN \
  cd ~/.asterius && \
  mkdir lib && \
  cd lib && \
  ../utils/make-packages.py && \
  rm -rf ghc && \
  cd .. && \
  stack --no-terminal update && \
  stack --no-terminal build \
    asterius && \
  ln -s $(stack path --local-install-root) ~/.asterius-local-install-root && \
  ln -s $(stack path --snapshot-install-root) ~/.asterius-snapshot-install-root && \
  ln -s $(stack path --compiler-bin) ~/.asterius-compiler-bin && \
  ahc-boot

RUN \
  apt purge -y \
    mawk && \
  apt autoremove --purge -y && \
  apt clean && \
  rm -rf -v \
    /root/.ahc-cabal \
    /root/.config \
    /root/.local/bin/stack \
    /root/.npm \
    /root/.stack/pantry \
    /root/.stack/programs/*/*.tar.xz \
    /tmp/* \
    /var/lib/apt/lists/* \
    /var/tmp/*

RUN \
  ahc --version && \
  alex --version && \
  cabal --version && \
  node --version && \
  wasm-opt --version && \
  ${WASI_SDK_PATH}/bin/wasm-ld --version
