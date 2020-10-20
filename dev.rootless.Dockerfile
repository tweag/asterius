ARG DEBIAN_FRONTEND=noninteractive
ARG USERNAME=asterius
ARG UID=1000

FROM debian:sid-slim AS rootless

ARG DEBIAN_FRONTEND
ARG USERNAME
ARG UID

RUN \
  apt update && \
  apt full-upgrade -y && \
  apt install -y \
    sudo && \
  apt autoremove --purge -y && \
  apt clean && \
  rm -rf -v \
    /tmp/* \
    /var/lib/apt/lists/* \
    /var/tmp/* && \
  useradd \
    --create-home \
    --shell /bin/bash \
    --uid ${UID} \
    ${USERNAME} && \
  echo "${USERNAME} ALL=(ALL) NOPASSWD:ALL" > /etc/sudoers.d/${USERNAME} && \
  chmod 0440 /etc/sudoers.d/${USERNAME}

USER ${USERNAME}

WORKDIR /home/${USERNAME}

FROM rootless

ARG DEBIAN_FRONTEND
ARG USERNAME
ARG UID

ENV \
  BROWSER=echo \
  LANG=C.UTF-8 \
  LC_ALL=C.UTF-8 \
  LC_CTYPE=C.UTF-8 \
  PATH=/home/${USERNAME}/.local/bin:/home/${USERNAME}/.nvm/versions/node/v15.0.0/bin:${PATH} \
  WASI_SDK_PATH=/opt/wasi-sdk

RUN \
  sudo apt update && \
  sudo apt full-upgrade -y && \
  sudo apt install -y \
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
  sudo mkdir -p ${WASI_SDK_PATH} && \
  (curl -L https://github.com/TerrorJack/wasi-sdk/releases/download/201014/wasi-sdk-11.5g3cbd9d212e9a-linux.tar.gz | sudo tar xz -C ${WASI_SDK_PATH} --strip-components=1) && \
  sudo apt autoremove --purge -y && \
  sudo apt clean && \
  sudo rm -rf -v \
    /tmp/* \
    /var/lib/apt/lists/* \
    /var/tmp/*

RUN \
  (curl https://raw.githubusercontent.com/nvm-sh/nvm/v0.36.0/install.sh | bash) && \
  bash -c ". ~/.nvm/nvm.sh && nvm install 15.0.0" && \
  echo "eval \"\$(direnv hook bash)\"" >> ~/.bashrc && \
  mkdir -p ~/.local/bin && \
  curl -L https://github.com/commercialhaskell/stack/releases/download/v2.5.1/stack-2.5.1-linux-x86_64-bin -o ~/.local/bin/stack && \
  chmod +x ~/.local/bin/stack && \
  curl -L https://downloads.haskell.org/~cabal/cabal-install-3.2.0.0/cabal-install-3.2.0.0-x86_64-unknown-linux.tar.xz | tar xJ -C ~/.local/bin 'cabal' && \
  npm install -g \
    @cloudflare/wrangler \
    0x \
    webpack@next \
    webpack-cli && \
  pip3 install \
    recommonmark \
    sphinx

COPY --chown=${UID} . /tmp/asterius

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
  cd ~ && \
  sudo rm -rf -v \
    ~/.npm \
    ~/.stack/pantry \
    ~/.stack/programs/*/*.tar.xz \
    /tmp/* \
    /var/tmp/*
