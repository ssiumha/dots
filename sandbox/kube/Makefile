.EXPORT_ALL_VARIABLES:
.PHONY: help
.DEFAULT_GOAL := help

_NAMESPACE ?= k3d-local

help:
	@awk 'BEGIN {FS = ":.*?## "} /^[a-zA-Z0-9_-]+:.*?## / {printf "\033[36m%-30s\033[0m %s\n", $$1, $$2}' $(MAKEFILE_LIST)

env:
	@echo _NAMESPACE=${_NAMESPACE}

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
	kubectl run postgres-client --rm -it --restart='Never' \
		-n ${_NAMESPACE} --image postgres:16 \
		--env="PGPASSWORD=postgres" --command \
		-- psql -h postgres -U postgres -d postgres

test-kafka-produce: ## Test kafka producer
	kubectl run kafka-producer --rm -it --restart='Never' \
		-n ${_NAMESPACE} --image confluentinc/cp-kafka:latest kafka-console-producer -- --bootstrap-server kafka:9092 --topic test-topic

test-kafka-consume: ## Test kafka consumer
	kubectl run kafka-consumer --rm -it --restart='Never' \
		-n ${_NAMESPACE} --image confluentinc/cp-kafka:latest kafka-console-consumer -- --bootstrap-server kafka:9092 --topic test-topic --from-beginning

test-vault: ## Test vault
	kubectl run curl --rm -it --restart='Never' \
		-n ${_NAMESPACE} --image alpine/curl -- vault:8200/v1/sys/health
