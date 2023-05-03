# syntax=docker/dockerfile:1.3-labs
FROM ubuntu:20.10

# Base
RUN apt update && apt upgrade -y

RUN <<EOF
  apt install -y tzdata locales ssh zsh curl less make unzip sudo iproute2
  apt clean
EOF

RUN <<EOF
  locale-gen en_US.UTF-8
EOF

# Install Docker
#   - software-properties-common depends python3
#   - docker-ce depends git
RUN <<EOF
  apt install -y apt-transport-https ca-certificates curl software-properties-common
  curl -fsSL https://download.docker.com/linux/ubuntu/gpg | apt-key add -
  add-apt-repository -y "deb https://download.docker.com/linux/ubuntu focal stable"
  apt update
  apt install -y docker-ce
EOF

# Install asdf build dependency
#   - build-essential depends gcc, g++, make, dpkg-dev
#   - ruby -> libssl, zlib1g
#   - tmux -> automake (aclocal, autoconf), pkg-config, libncurses-dev, byacc
RUN <<EOF
  apt install -y \
      build-essential automake pkg-config \
      libssl-dev zlib1g-dev \
      libncurses-dev byacc
  apt clean
EOF

# Create default user
RUN useradd --create-home --shell /bin/zsh --home-dir /home/me me
COPY <<EOF /etc/sudoers.d/sudome
me ALL=(ALL) NOPASSWD: ALL
EOF

USER me

# ASDF
COPY --chown=me ./asdf /home/me/dotfiles/asdf
WORKDIR /home/me/dotfiles/asdf

COPY --chown=me <<EOF /home/me/.tool-versions
neovim 0.5.0
tmux 3.2
fzf 0.27.2
ruby 3.0.0
EOF

RUN <<EOF
  make install
  make init-plugins
  make install-tools
EOF

# Dotfiles
COPY --chown=me . /home/me/dotfiles
WORKDIR /home/me/dotfiles

RUN <<EOF
  make prepare-local-directory
  make install-zshrc
  make install-gitconfig
  make install-vimrc
  make install-nvimrc
EOF

# Run Entrypoint
USER root
WORKDIR /

RUN <<EOF
  echo "TZ=Asia/Seoul" >> /etc/environment
  echo "__IN_DOCKER=true" >> /etc/environment
EOF

COPY ./docker/entrypoint.sh /entrypoint.sh
ENTRYPOINT ["/entrypoint.sh"]
