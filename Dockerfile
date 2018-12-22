FROM debian:unstable

COPY asterius /root/asterius/asterius
COPY binaryen /root/asterius/binaryen
COPY ghc-toolkit /root/asterius/ghc-toolkit
COPY inline-js /root/asterius/inline-js
COPY npm-utils /root/asterius/npm-utils
COPY wabt /root/asterius/wabt
COPY wasm-toolkit /root/asterius/wasm-toolkit
COPY stack.yaml /root/asterius/stack.yaml
COPY utils/v8-node.py /tmp/v8-node.py
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
    libffi-dev \
    libgmp-dev \
    libncurses-dev \
    libnuma-dev \
    make \
    python-minimal \
    python3-minimal \
    unzip \
    xz-utils \
    zlib1g-dev && \
  mkdir /root/.local && \
  curl $(python3 /tmp/v8-node.py) -o /tmp/v8-node.zip && \
  unzip -q /tmp/v8-node.zip -d /root/.local && \
  curl -L https://get.haskellstack.org/stable/linux-x86_64.tar.gz | tar xz --wildcards --strip-components=1 -C /root/.local/bin '*/stack' && \
  stack --no-terminal install asterius wabt && \
  stack --no-terminal exec ahc-boot && \
  apt purge -y \
    automake \
    cmake \
    curl \
    g++ \
    make \
    python-minimal \
    python3-minimal \
    unzip \
    xz-utils && \
  apt autoremove --purge -y && \
  rm -rf \
    /root/.stack/programs/x86_64-linux/*.tar.xz \
    /var/lib/apt/lists/* \
    /tmp/* && \
  mv /root/.stack/programs /tmp/ && \
  rm -rf /root/.stack && \
  mkdir /root/.stack && \
  mv /tmp/programs /root/.stack/
