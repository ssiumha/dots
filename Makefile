.EXPORT_ALL_VARIABLES:

.PHONY: help test
help:
	@awk 'BEGIN {FS = ":.*?##"} \
		/^[^_.#][a-zA-Z0-9_-]+:($$| .*?)/ {printf "\033[36m%-30s\033[0m %s\n", $$1, $$2}' \
		$(MAKEFILE_LIST) \
		| sort
.DEFAULT_GOAL := help

SHELL := /usr/bin/env bash
SHELL_OPTIONS ?= -euo pipefail

DOTFILES:=$$HOME/dotfiles

# read -> $REPLY not working
CHECK_Y=@read line; if [ $$line != "y" ]; then echo 1 ; fi

# PREPARE {{{
prepare-local-directory:
	mkdir -p "$$HOME/.local/"{repo,bin,vim,zsh/completion,zsh/zplug,emacs/tmp}
	mkdir -p "$$HOME/.local/"{sh,share/man/man1}
	mkdir -p "$$HOME/.local/vim/tmp/"{undo,backup,swap}

# }}}

# INSTALL {{{

# TODO: -sFh is BSD ln option
symlink-config: CONFIG_PATH=$$HOME/dotfiles/config
symlink-config:
	touch "$$HOME/.gitconfig"; # save to user.name, user.email
	mkdir -p "$$HOME/.config";
	for CONFIG_DIR in $$(ls "${CONFIG_PATH}"); \
	do \
		echo "${CONFIG_PATH}/$$CONFIG_DIR -> $$HOME/.config/$$CONFIG_DIR"; \
		ln -sFh "${CONFIG_PATH}/$$CONFIG_DIR" "$$HOME/.config/$$CONFIG_DIR"; \
	done

install-vimrc:
	@if [[ -s "$$HOME/.vimrc" ]]; \
	then echo -e "\033[91malready exists vimrc\033[0m"; \
	else echo "source $(DOTFILES)/vimrc" > "$$HOME/.vimrc"; \
	fi

install-zshrc:
	@if [[ -s "$$HOME/.zshrc" ]]; \
	then echo -e "\033[91malready exists zshrc\033[0m"; \
	else echo "source $(DOTFILES)/zshrc" > "$$HOME/.zshrc"; \
	fi

# }}}

# UTIL {{{

util-install-z:
	curl https://raw.githubusercontent.com/rupa/z/master/z.sh > ~/.local/sh/z.sh
	curl https://raw.githubusercontent.com/rupa/z/master/z.1 > ~/.local/share/man/man1/z.1
	if [[ ! $$(grep "source ~/.local/sh/z.sh" ~/.zshrc) ]]; \
		then echo "source ~/.local/sh/z.sh" >> $$HOME/.zshrc; \
	fi

util-install-fzy:
	git clone --depth=1 "https://github.com/jhawthorn/fzy.git" "$$HOME/.local/repo/fzy" && \
		cd "$$HOME/.local/repo/fzy" && make && make install -e "PREFIX=$$HOME/.local/" && cd -

util-install-zplug:
	echo -e 'install zplug? [y/N]: '; \
	read line && if [[ $$line = "y" ]]; \
	then ZPLUG_HOME=$$HOME/.local/zsh/zplug \
		&& git clone https://github.com/zplug/zplug $$ZPLUG_HOME && source $$ZPLUG_HOME/init.zsh; \
	fi

util-install-zinit:
	echo -e 'install zinit? [y/N]: '; \
	read line && if [[ $$line = "y" ]]; \
	then ZINIT_HOME=$$HOME/.local/zsh/zinit \
		&& git clone --depth=5 https://github.com/zdharma-continuum/zinit $$ZINIT_HOME/bin; \
	fi

util-install-brew:
	@if ! command -v brew &> /dev/null; \
		then /bin/bash -c "$$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install.sh)"; \
	fi

util-install-ack:
	curl -fL http://beyondgrep.com/ack-3.5.0 > $HOME/.local/bin/ack && chmod 0755 $HOME/.local/bin/ack

util-install-q: util-install-brew
util-install-q:
	brew install q

util-install-kube-forwarder: util-install-brew
util-install-kube-forwarder:
	brew install kube-forwarder

util-install-all: util-install-z util-install-diffhighlight
util-install-all: util-install-vimplug util-install-zplug
util-install-all: util-install-brew
util-install-all: util-install-ack
util-install-all: util-install-q

#util-install-volt:
#[[ ! -s "$HOME/.local/bin/volt" ]] && \
#    printf 'install volt...' && \
#        curl -s https://api.github.com/repos/vim-volt/volt/releases/latest \
#        | grep browser_download_url \
#        | case "$OSTYPE" in \
#            darwin*) grep darwin-amd64 ;;
#            *) grep linux-$( (uname -a | grep 'x86-64' > /dev/null) && echo 'amd64' || echo '386') ;; esac \
#        | cut -d '"' -f 4 \
#        | wget -i - -O "$HOME/.local/bin/volt" && chmod +x "$HOME/.local/bin/volt"

# }}}

# OPT {{{

opt-install-guremkim:
	brew cask install gureumkim \
		&& open '/Library/Input Methods/Gureum.app'

opt-setting-iterm2:
	@if [[ "$$OSTYPE" == darwin* && $$(defaults domains | grep com.googlecode.iterm2) ]]; \
	then \
		echo -e 'load item2 setting? [y/N]: '; \
		read line; if [[ $$line = "y" ]]; \
		then \
			defaults write com.googlecode.iterm2.plist PrefsCustomFolder -string "$(DOTFILES)/iterm2" \
				&& defaults write com.googlecode.iterm2.plist LoadPrefsFromCustomFolder -bool true; \
		fi \
	fi


define _CSS_BODY
#main-window[tabsintitlebar="true"]:not([extradragspace="true"]) #TabsToolbar {
  opacity: 0;
  pointer-events: none;
}

#main-window:not([tabsintitlebar="true"]) #TabsToolbar {
  visibility: collapse !important;
}
endef
opt-firefox-hidetab: export CSS_BODY:=$(_CSS_BODY)
opt-firefox-hidetab:
	cd "$$(find "$$HOME/Library/Application Support/Firefox/Profiles/" | egrep 'default-release$$' | head -n1)" \
	&& if [[ $$(pwd) =~ "default-release" ]]; \
	then \
		pwd; \
		mkdir -p chrome; \
		echo "$$CSS_BODY" > chrome/UserChrome.css; \
	else \
		echo 'opt-firefox-hidetab: failed cd'; \
	fi

opt-firefox-hidetab-remove:
	cd "$$(find "$$HOME/Library/Application Support/Firefox/Profiles/" | egrep 'default-release$$' | head -n1)" \
	&& if [[ $$(pwd) =~ "default-release" ]]; \
	then \
		pwd; \
		mv chrome/UserChrome{,_}.css; \
	else \
		echo 'opt-firefox-hidetab: not found UserChrome.css'; \
	fi

# }}}

build:
	docker buildx build --progress=plain \
		-f docker/Dockerfile -t home .

run:
	docker run -it --rm \
		--name home \
		--hostname home-docker \
		-v /var/run/docker.sock:/var/run/docker.sock \
		-v $$HOME/dotfiles:/home/me/dotfiles \
		-v $$HOME/.ssh:/home/me/.ssh \
		-e HOST_HOME=$$HOME \
		-e HOST_USER_ID=$$(id -u $$USER) \
		-e HOST_GROUP_ID=$$(id -g $$USER) \
		home:latest

run-root:
	docker run -it --rm \
		--name home \
		-v /var/run/docker.sock:/var/run/docker.sock \
		home:latest root

test:
	$(if $(shell [[ "$$OSTYPE" = darwin* ]]), echo 1, echo 2)

#[[ "$$OSTYPE" == darwin* && $(defaults domains | grep com.googlecode.iterm2) ]] && make load-setting-iterm2

# STATEMENT MEMO
# if-statement:
#	 $(if $(command ...),
#	 	@echo 1, \
#	 	@echo 0 \
#	 )
#
#	 <<<"$(<in)" awk '{if(/foo/){x=sub(/foo/, "bar", $0)};print}END{if(x!=1){print "bar"}}' >in

# vim: fdm=marker
