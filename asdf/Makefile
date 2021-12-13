.EXPORT_ALL_VARIABLES:

SHELL := /usr/bin/env bash
SHELL_OPTIONS ?= -euo pipefail

ASDF_PATH := ${HOME}/.asdf

help:
	@echo "install        - clone asdf-vm/asdf.git to $$ASDF_PATH"
	@echo 'link-versions  - create symlink tool-versions'
	@echo 'init-plugins   - initialize my common plugins'
	@echo 'install-tools  - `asdf install`'
.PHONY: help

install:
	@if [[ -s "$$ASDF_PATH" ]]; \
	then echo -e "\033[91malready exists asdf\033[0m"; \
	else git clone https://github.com/asdf-vm/asdf.git $$ASDF_PATH \
			&& cd $$ASDF_PATH && git checkout "$$(git describe --abbrev=0 --tags)"; \
	fi

link-tool-versions:
	@if [[ -s "$$HOME/.tool-versions" ]]; \
	then echo -e "\033[91malready exists .tool-versions\033[0m"; \
	else ln -s "$$HOME/dotfiles/asdf/tool-versions" "$$HOME/.tool-versions"; \
	fi

init-plugins:
	source $$ASDF_PATH/asdf.sh && \
		awk '!/^(#|$$)/{ cmd = "asdf plugin add " $$1; system(cmd); }' tool-versions

install-tools:
	source $$ASDF_PATH/asdf.sh && asdf install
