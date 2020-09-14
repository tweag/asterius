FROM debian:sid-slim

ARG DEBIAN_FRONTEND=noninteractive
ARG USERNAME=asterius
ARG UID=1000

ENV \
  LANG=C.UTF-8 \
  LC_ALL=C.UTF-8 \
  LC_CTYPE=C.UTF-8 \
  PATH=/home/${USERNAME}/.local/bin:/home/${USERNAME}/.nvm/versions/node/v14.10.1/bin:${PATH}

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
  useradd \
    --create-home \
    --shell /bin/bash \
    --uid ${UID} \
    asterius && \
  echo "${USERNAME} ALL=(ALL) NOPASSWD:ALL" > /etc/sudoers.d/${USERNAME} && \
  chmod 0440 /etc/sudoers.d/${USERNAME}

USER ${USERNAME}

WORKDIR /home/${USERNAME}

RUN \
  (curl https://raw.githubusercontent.com/nvm-sh/nvm/v0.35.3/install.sh | bash) && \
  bash -c ". ~/.nvm/nvm.sh && nvm install 14.10.0" && \
  echo "eval \"\$(direnv hook bash)\"" >> ~/.bashrc && \
  mkdir -p ~/.local/bin && \
  curl -L https://github.com/commercialhaskell/stack/releases/download/v2.3.3/stack-2.3.3-linux-x86_64-bin -o ~/.local/bin/stack && \
  chmod +x ~/.local/bin/stack && \
  curl -L https://downloads.haskell.org/~cabal/cabal-install-3.2.0.0/cabal-install-3.2.0.0-x86_64-unknown-linux.tar.xz | tar xJ -C ~/.local/bin 'cabal' && \
  npm install -g \
    0x@4.9.1 \
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
  cd /home/${USERNAME} && \
  sudo rm -rf -v \
    /home/${USERNAME}/.npm \
    /home/${USERNAME}/.stack/pantry \
    /home/${USERNAME}/.stack/programs/*/*.tar.xz \
    /tmp/* \
    /var/tmp/*
