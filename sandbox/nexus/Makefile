.EXPORT_ALL_VARIABLES:
.PHONY: help
.DEFAULT_GOAL := help

help:
	@awk 'BEGIN {FS = ":.*?## "} /^[a-zA-Z0-9_-]+:.*?## / {printf "\033[36m%-30s\033[0m %s\n", $$1, $$2}' $(MAKEFILE_LIST)

get-admin:
	docker compose exec nexus cat /nexus-data/admin.password

create-gpg-key:
	# https://help.sonatype.com/en/apt-repositories.html#hosting-apt-repositories
	@echo '  docker run -it --rm -v $$(PWD)/keys:/keys ubuntu:22.04 bash'
	@echo '  apt update && apt install gpg'
	@echo '  cd /keys'
	@echo '  gpg --gen-key'
	@echo '  gpg --list-keys'
	@echo '  gpg --armor --output public.gpg.key --export <key-id>'
	@echo '  gpg gpg --armor --output private.gpg.key --export-secret-key <key-id>'

deploy-package:
	@echo "curl -u 'admin:password' -H 'Content-Type: multipart/form-data' --data-binary "@./test.deb" "http://localhost:8081/repository/apt-hosted/""
