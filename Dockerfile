#!/usr/bin/env -S docker buildx build --platform linux/amd64 -t dotker:latest --progress plain . -f

FROM ubuntu:24.04

RUN apt update && apt install -y curl git tzdata zsh sudo

SHELL ["bash", "-l", "-c"]

# install mise
RUN useradd --shell /usr/bin/zsh --create-home dot
RUN echo "dot ALL=NOPASSWD: ALL" > /etc/sudoers.d/dot

USER dot

# ## mise base
# RUN apt update \
#     && apt install -y --no-install-recommends \
#       unzip
#
# ## language base
# RUN apt update \
#     && apt install -y --no-install-recommends \
#       ruby-build libffi-dev libyaml-dev ruby-mimemagic
#
# ## runtime base
# RUN apt update \
#     && apt install -y --no-install-recommends \
#       libmysqlclient-dev mysql-client \
#       libsqlite3-dev
# mariadb-client libjemalloc2

# RUN rm -rf /var/lib/apt/lists/*

RUN curl https://mise.run | sh

COPY <<EOF /home/dot/.zshrc
export PATH="/home/dot/.local/bin:$PATH"

eval "$(mise activate zsh)"

export PROMPT='%F{blue}%n%f> %F{yellow}%~%f %F{red}%#%f '
EOF

VOLUME /root/.local
VOLUME /root/dotfiles
VOLUME /volumes
VOLUME /cache

ENV TZ=Asia/Seoul
ENV MISE_LEGACY_VERSION_FILE_DISABLE_TOOLS=python
ENV BUNDLE_PATH=/cache/bundle
ENV BUNDLE_CACHE_PATH=/cache/bundle/cache

WORKDIR /home/dot
ENTRYPOINT "zsh"
CMD "-l"
