FROM ubuntu

RUN apt-get update && apt-get install -y curl
RUN curl -o- https://raw.githubusercontent.com/nvm-sh/nvm/v0.39.7/install.sh | bash

SHELL ["/bin/bash", "-c"]

RUN source ~/.nvm/nvm.sh && nvm install 20

COPY <<EOF /entrypoint.sh
#!/bin/bash
source ~/.nvm/nvm.sh

node --version
EOF

RUN chmod +x /entrypoint.sh

ENTRYPOINT /entrypoint.sh
