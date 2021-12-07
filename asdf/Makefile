.EXPORT_ALL_VARIABLES:

SHELL := /usr/bin/env bash
SHELL_OPTIONS ?= -euo pipefail

help:
	@echo 'install        - clone asdf-vm/asdf.git to ~/.asdf'
	@echo 'link-versions  - create symlink tool-versions'
	@echo 'init-plugins   - initialize my common plugins'
	@echo 'install-tools  - `asdf install`'
.PHONY: help

install:
	@if [[ -s "$$HOME/.asdf" ]]; \
	then echo -e "\033[91malready exists asdf\033[0m"; \
	else git clone https://github.com/asdf-vm/asdf.git ~/.asdf \
			&& cd ~/.asdf && git checkout "$$(git describe --abbrev=0 --tags)"; \
	fi

link-versions:
	@if [[ -s "$$HOME/.tool-versions" ]]; \
	then echo -e "\033[91malready exists .tool-versions\033[0m"; \
	else ln -s "$$HOME/dotfiles/asdf/tool-versions" "$$HOME/.tool-versions"; \
	fi

init-plugins: PLUGIN_LIST = jq yq neovim tmux fzf \
                            awscli kubectl k9s helm terraform \
                            ruby bundler
init-plugins:
	source $$HOME/.asdf/asdf.sh && \
		for x in ${PLUGIN_LIST}; do asdf plugin add $$x; done;

install-tools:
	source $$HOME/.asdf/asdf.sh && asdf install
