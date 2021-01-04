ARG DEBIAN_FRONTEND=noninteractive
ARG USERNAME=asterius
ARG UID=1000

FROM debian:sid AS rootless

ARG DEBIAN_FRONTEND
ARG USERNAME
ARG UID

RUN \
  rm /etc/apt/apt.conf.d/docker-clean && \
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

ARG NODE_VER=15.5.1

ENV \
  BROWSER=echo \
  LANG=C.UTF-8 \
  PATH=/home/${USERNAME}/.local/bin:/home/${USERNAME}/.nvm/versions/node/v${NODE_VER}/bin:${PATH} \
  WASI_SDK_PATH=/opt/wasi-sdk

RUN \
  sudo apt update && \
  sudo apt full-upgrade -y && \
  sudo apt install -y \
    alex \
    automake \
    bash-completion \
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
  sudo mkdir -p ${WASI_SDK_PATH} && \
  (curl -L https://github.com/TerrorJack/wasi-sdk/releases/download/201202/wasi-sdk-11.9gb368b8b12ee6-linux.tar.gz | sudo tar xz -C ${WASI_SDK_PATH} --strip-components=1) && \
  sudo apt autoremove --purge -y && \
  sudo apt clean && \
  sudo rm -rf -v \
    /tmp/* \
    /var/lib/apt/lists/* \
    /var/tmp/*

RUN \
  (curl https://raw.githubusercontent.com/nvm-sh/nvm/v0.37.2/install.sh | bash) && \
  bash -i -c "nvm install ${NODE_VER}" && \
  bash -i -c "npm install -g @cloudflare/wrangler webpack webpack-cli" && \
  mkdir -p ~/.local/bin && \
  curl -L https://github.com/commercialhaskell/stack/releases/download/v2.5.1/stack-2.5.1-linux-x86_64-bin -o ~/.local/bin/stack && \
  chmod +x ~/.local/bin/stack && \
  curl http://oleg.fi/cabal-install-3.4.0.0-rc4/cabal-install-3.4.0.0-x86_64-ubuntu-16.04.tar.xz | tar xJ -C ~/.local/bin --wildcards '*/cabal' && \
  echo "eval \"\$(stack --bash-completion-script stack)\"" >> ~/.bashrc && \
  echo "eval \"\$(direnv hook bash)\"" >> ~/.bashrc && \
  pip3 install \
    recommonmark \
    sphinx

RUN \
  stack --no-terminal update && \
  stack --no-terminal --resolver lts-16.27 install \
    brittany \
    ghcid \
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
