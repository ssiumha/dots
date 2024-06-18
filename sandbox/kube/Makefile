.EXPORT_ALL_VARIABLES:
.PHONY: help
.DEFAULT_GOAL := help

help:
	@awk 'BEGIN {FS = ":.*?## "} /^[a-zA-Z0-9_-]+:.*?## / {printf "\033[36m%-30s\033[0m %s\n", $$1, $$2}' $(MAKEFILE_LIST)

install-k3d: ## Install k3d
	@if [ -z $(shell which k3d) ]; then \
		echo "Installing k3d"; \
		curl -s https://raw.githubusercontent.com/rancher/k3d/main/install.sh | bash; \
	else \
		echo "k3d already installed"; \
	fi

create-cluster: ## Create a k3d cluster
	@if [ -z $(shell k3d cluster list | grep mycluster) ]; then \
		k3d cluster create mycluster; \
	else \
		echo "Cluster `mycluster` already exists"; \
	fi

connect-postgres: ## Connect to the postgres database
	kubectl run postgres-client --rm --tty -i --restart='Never' \
		-n k3d-local --image postgres:16 \
		--env="PGPASSWORD=postgres" --command \
		-- psql -h postgres.k3d-local.svc.cluster.local -U postgres -d postgres

