FROM debian:testing

COPY asterius /root/asterius/asterius
COPY binaryen /root/asterius/binaryen
COPY ghc-toolkit /root/asterius/ghc-toolkit
COPY inline-js /root/asterius/inline-js
COPY wabt /root/asterius/wabt
COPY wasm-toolkit /root/asterius/wasm-toolkit
COPY stack.yaml /root/asterius/stack.yaml
ENV \
  DEBIAN_FRONTEND=noninteractive \
  LANG=C.UTF-8 \
  LC_ALL=C.UTF-8 \
  PATH=/root/.local/bin:/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin
WORKDIR /root/asterius

RUN \
  apt update && \
  apt full-upgrade -y && \
  apt install -y \
    automake \
    cmake \
    curl \
    g++ \
    gcc \
    jq \
    libffi-dev \
    libgmp-dev \
    libncurses-dev \
    libnuma-dev \
    make \
    python-minimal \
    xz-utils \
    zlib1g-dev && \
  mkdir /root/.local && \
  export NODE_VER=$(curl https://nodejs.org/download/v8-canary/index.json | jq -r 'map(select(.files[] | contains("linux-x64"))) | .[0].version') && \
  curl https://nodejs.org/download/v8-canary/$NODE_VER/node-$NODE_VER-linux-x64.tar.xz | tar xJ --strip-components=1 -C /root/.local && \
  curl -L https://get.haskellstack.org/stable/linux-x86_64.tar.gz | tar xz --wildcards --strip-components=1 -C /root/.local/bin '*/stack' && \
  stack --no-terminal install asterius wabt && \
  stack --no-terminal exec ahc-boot && \
  apt purge -y \
    automake \
    cmake \
    curl \
    g++ \
    jq \
    make \
    python-minimal \
    xz-utils && \
  apt autoremove --purge -y && \
  rm -rf /var/lib/apt/lists/* && \
  rm /root/.stack/programs/x86_64-linux/*.tar.xz && \
  mv /root/.stack/programs /tmp/ && \
  rm -rf /root/.stack && \
  mkdir /root/.stack && \
  mv /tmp/programs /root/.stack/
