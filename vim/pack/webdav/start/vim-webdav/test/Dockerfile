FROM alpine:latest

# Install required packages
RUN apk add --no-cache \
    vim \
    tmux \
    curl \
    perl \
    perl-uri \
    bash \
    ruby

# Create vim directory structure
RUN mkdir -p /root/.vim/pack/webdav/start/vim-webdav

# Copy plugin files
COPY plugin /root/.vim/pack/webdav/start/vim-webdav/plugin
COPY test /root/.vim/pack/webdav/start/vim-webdav/test

# Configure tmux
RUN echo "set -g default-terminal 'screen-256color'" > /root/.tmux.conf && \
    echo "set -g escape-time 0" >> /root/.tmux.conf

# Set working directory
WORKDIR /root

# Start tmux server in background and keep container running
CMD tmux new-session -d -s main && tail -f /dev/null