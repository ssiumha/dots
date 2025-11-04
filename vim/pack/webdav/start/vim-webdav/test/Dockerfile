FROM alpine:latest

# Install required packages including nginx with WebDAV support
RUN apk add --no-cache \
    vim \
    tmux \
    curl \
    perl \
    perl-uri \
    bash \
    ruby \
    nginx \
    nginx-mod-http-dav-ext

# Create vim directory structure
RUN mkdir -p /root/.vim/pack/webdav/start/vim-webdav

# Copy plugin files
COPY plugin /root/.vim/pack/webdav/start/vim-webdav/plugin
COPY syntax /root/.vim/pack/webdav/start/vim-webdav/syntax
COPY ftplugin /root/.vim/pack/webdav/start/vim-webdav/ftplugin
COPY test /root/.vim/pack/webdav/start/vim-webdav/test

# Configure tmux
RUN echo "set -g default-terminal 'screen-256color'" > /root/.tmux.conf && \
    echo "set -g escape-time 0" >> /root/.tmux.conf

# Setup nginx for WebDAV
RUN mkdir -p /var/webdav /var/log/nginx /run/nginx
COPY test/nginx.conf /etc/nginx/nginx.conf
COPY test/init-webdav.sh /root/init-webdav.sh
COPY test/start.sh /root/start.sh
RUN chmod +x /root/init-webdav.sh /root/start.sh && /root/init-webdav.sh

# Set working directory
WORKDIR /root

# Start services
CMD ["/bin/bash", "/root/start.sh"]