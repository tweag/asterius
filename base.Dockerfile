FROM debian:sid-slim

ARG DEBIAN_FRONTEND=noninteractive

ENV \
  LANG=C.UTF-8 \
  LC_ALL=C.UTF-8 \
  LC_CTYPE=C.UTF-8 \
  PATH=/root/.asterius-local-install-root/bin:/root/.asterius-snapshot-install-root/bin:/root/.asterius-compiler-bin:/root/.local/bin:/root/.nvm/versions/node/v14.12.0/bin:/opt/wasi-sdk/bin:${PATH}

RUN \
  apt update && \
  apt full-upgrade -y && \
  apt install -y \
    automake \
    build-essential \
    binaryen \
    curl \
    gawk \
    git \
    libffi-dev \
    libgmp-dev \
    libncurses-dev \
    libtinfo5 \
    python3-minimal \
    zlib1g-dev && \
  curl \
    -L \
    -o /tmp/wasi-sdk.deb \
    https://github.com/WebAssembly/wasi-sdk/releases/download/wasi-sdk-11/wasi-sdk_11.0_amd64_ubuntu20.04.deb && \
  dpkg -i /tmp/wasi-sdk.deb && \
  rm /tmp/wasi-sdk.deb && \
  cp \
    /etc/skel/.bash_logout \
    /etc/skel/.bashrc \
    /etc/skel/.profile \
    /root

WORKDIR /root

RUN \
  (curl https://raw.githubusercontent.com/nvm-sh/nvm/v0.36.0/install.sh | bash) && \
  bash -c ". ~/.nvm/nvm.sh && nvm install 14.12.0" && \
  npm config set unsafe-perm true && \
  npm install -g \
    @cloudflare/wrangler \
    webpack@next \
    webpack-cli && \
  mkdir -p ~/.local/bin && \
  curl -L https://github.com/commercialhaskell/stack/releases/download/v2.5.0.1/stack-2.5.0.1-linux-x86_64-bin -o ~/.local/bin/stack && \
  chmod +x ~/.local/bin/stack && \
  curl -L https://downloads.haskell.org/~cabal/cabal-install-3.2.0.0/cabal-install-3.2.0.0-x86_64-unknown-linux.tar.xz | tar xJ -C ~/.local/bin 'cabal'

COPY . /root/.asterius

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
  ahc-boot

RUN \
  apt purge -y \
    mawk && \
  apt autoremove --purge -y && \
  apt clean && \
  find /root \( -name "*.p_hi" -o -name "*.p_o" -o -name "*_p.a" \) -type f -delete && \
  mv \
    /root/.asterius-local-install-root/bin \
    /root/.asterius-local-install-root/share \
    /tmp && \
  rm -rf -v \
    /root/.asterius \
    /root/.asterius-compiler-bin/../share \
    /root/.ahc-cabal \
    /root/.config \
    /root/.local/bin/stack \
    /root/.npm \
    /root/.stack/programs/*/*.tar.xz \
    /var/lib/apt/lists/* \
    /var/tmp/* && \
  mkdir -p $(realpath -m /root/.asterius-local-install-root) && \
  mv \
    /tmp/bin \
    /tmp/share \
    /root/.asterius-local-install-root && \
  mv \
    /root/.asterius-snapshot-install-root/bin \
    /root/.asterius-snapshot-install-root/share \
    /root/.stack/programs \
    /tmp && \
  rm -rf -v /root/.stack && \
  mkdir -p $(realpath -m /root/.asterius-snapshot-install-root) && \
  mv \
    /tmp/bin \
    /tmp/share \
    /root/.asterius-snapshot-install-root && \
  mv \
    /tmp/programs \
    /root/.stack && \
  rm -rf -v \
    /tmp/*

RUN \
  ahc --version && \
  alex --version && \
  cabal --version && \
  node --version && \
  wasm-opt --version
