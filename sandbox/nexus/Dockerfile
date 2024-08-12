FROM ubuntu:22.04

RUN apt-get update && apt-get install -y gnupg

COPY <<-EOF /tmp/apt-jammy.gpg.key
-----BEGIN PGP PUBLIC KEY BLOCK-----

37 lines BASE64 value...
-----END PGP PUBLIC KEY BLOCK-----
EOF

RUN cat /tmp/apt-jammy.gpg.key | gpg --dearmor > /etc/apt/trusted.gpg.d/apt-jammy.gpg && rm /tmp/apt-jammy.gpg.key
RUN rm -rf /var/lib/apt/lists/*

COPY <<EOF /etc/apt/sources.list
deb http://host.docker.internal:8081/repository/apt-jammy jammy main
# deb http://host.docker.internal:8081/repository/apt-jammy jammy main restricted
# deb http://host.docker.internal:8081/repository/apt-jammy jammy universe
# deb http://host.docker.internal:8081/repository/apt-jammy jammy multiverse
# deb http://host.docker.internal:8081/repository/apt-jammy jammy-updates main restricted
# deb http://host.docker.internal:8081/repository/apt-jammy jammy-updates multiverse
# deb http://host.docker.internal:8081/repository/apt-jammy jammy-updates universe
# deb http://host.docker.internal:8081/repository/apt-jammy jammy-backports main restricted universe multiverse
# deb http://host.docker.internal:8081/repository/apt-jammy jammy-security main restricted
# deb http://host.docker.internal:8081/repository/apt-jammy jammy-security universe
# deb http://host.docker.internal:8081/repository/apt-jammy jammy-security multiverse
EOF

CMD ["bash"]
