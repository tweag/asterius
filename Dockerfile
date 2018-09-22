FROM debian:unstable

COPY asterius /root/asterius
COPY binaryen /root/binaryen
COPY ghc-toolkit /root/ghc-toolkit
COPY wasm-toolkit /root/wasm-toolkit
COPY stack.yaml /root/stack.yaml
ENV \
  DEBIAN_FRONTEND=noninteractive \
  LANG=C.UTF-8 \
  LC_ALL=C.UTF-8 \
  PATH=/root/.local/bin:/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin
WORKDIR /root

RUN \
  apt update && \
  apt install -y \
    apt-transport-https \
    curl \
    gnupg && \
  curl https://deb.nodesource.com/gpgkey/nodesource.gpg.key | apt-key add - && \
  echo "deb https://deb.nodesource.com/node_10.x sid main" >> /etc/apt/sources.list && \
  apt update && \
  apt dist-upgrade -y && \
  apt install -y \
    automake \
    cmake \
    g++ \
    libffi-dev \
    libgmp-dev \
    libncurses-dev \
    libnuma-dev \
    make \
    nodejs \
    xz-utils \
    zlib1g-dev && \
  apt autoremove -y && \
  apt clean -y && \
  mkdir -p ~/.local/bin && \
  curl -L https://get.haskellstack.org/stable/linux-x86_64.tar.gz | tar xz --wildcards --strip-components=1 -C ~/.local/bin '*/stack' && \
  stack --no-terminal install asterius && \
  stack --no-terminal exec ahc-boot && \
  rm /root/.stack/programs/x86_64-linux/*.tar.xz && \
  mv /root/.stack/programs /tmp/ && \
  rm -rf /root/.stack && \
  mkdir /root/.stack && \
  mv /tmp/programs /root/.stack/
