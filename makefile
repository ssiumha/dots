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

init-local-directory:
	mkdir -p "$$HOME/.local/"{repo,bin,vim,zsh/completion,zsh/zplug,emacs/tmp}
	mkdir -p "$$HOME/.local/"{sh,share/man/man1}
	mkdir -p "$$HOME/.local/vim/tmp/"{undo,backup,swap}

# INSTALL {{{

install-gitconfig:
	@if [[ -s "$$HOME/.gitconfig" ]]; \
	then echo -e "\033[91malready exists gitconfig\033[0m"; \
	else git config --global include.path "$(DOTFILES)/gitconfig"; \
	fi

install-tumxconfig:
	@if [[ -s "$$HOME/.tmux.conf" ]]; \
	then echo -e "\033[91malready exists tmuxconfig\033[0m"; \
	else echo "source-file $(DOTFILES)/tmux.conf" > $$HOME/.tmux.conf; \
	fi

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

install-nvimrc:
	@if [[ -s "$$HOME/.config/nvim" ]]; \
	then echo -e "\033[91malready exists nvimrc\033[0m"; \
	else mkdir -p "$$HOME/.config/nvim" \
		&& echo "set rtp+=$$HOME/.vim" > "$$HOME/.config/nvim/init.vim" \
		&& echo "source $$HOME/.vimrc" >> "$$HOME/.config/nvim/init.vim" \
	fi

# }}}

# UTIL {{{

util-install-z:
	curl https://raw.githubusercontent.com/rupa/z/master/z.sh > ~/.local/sh/z.sh
	curl https://raw.githubusercontent.com/rupa/z/master/z.1 > ~/.local/share/man/man1/z.1
	if [[ ! $$(grep "source ~/.local/sh/z.sh" ~/.zshrc) ]]; \
		then echo "source ~/.local/sh/z.sh" >> $$HOME/.zshrc; \
	fi

util-install-vimplug:
	curl -fLo ~/.vim/autoload/plug.vim --create-dirs \
		https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim

util-install-diffhighlight:
	curl https://raw.githubusercontent.com/git/git/master/contrib/diff-highlight/{DiffHighlight.pm,diff-highlight.perl}) > "~/.local/bin/diff-highlight" \
		&& chmod +x "~/.local/bin/diff-highlight"

util-install-fzy:
	git clone --depth=1 "https://github.com/jhawthorn/fzy.git" "$$HOME/.local/repo/fzy" && \
		cd "$$HOME/.local/repo/fzy" && make && make install -e "PREFIX=$$HOME/.local/" && cd -

util-install-zplug:
	echo -e 'load item2 setting? [y/N]: '; \
	read line && if [[ $$line = "y" ]]; \
	then ZPLUG_HOME=$$HOME/.local/zsh/zplug \
		&& git clone https://github.com/zplug/zplug $$ZPLUG_HOME && source $$ZPLUG_HOME/init.zsh; \
	fi



# TODO: pure linux install case (need to build?)

util-install-brew:
	@if ! command -v brew &> /dev/null; \
		then /bin/bash -c "$$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install.sh)"; \
	fi

util-install-ack: util-install-brew
util-install-ack:
	brew install ack
	#curl -fL http://beyondgrep.com/ack-2.14-single-file > $HOME/.local/bin/ack && chmod 0755 $HOME/.local/bin/ack

util-install-ag: util-install-brew
util-install-ag:
	brew install ag

util-install-fzf: util-install-brew
util-install-fzf:
	brew install fzf

#        git clone --depth=1 "git@github.com:junegunn/fzf.git" "$HOME/.local/repo/fzf" && \
        $HOME/.local/repo/fzf/install --bin && \
        ln -s $HOME/.local/repo/fzf/bin/fzf $HOME/.local/bin/fzf && \
        ln -s $HOME/.local/repo/fzf/bin/fzf-tmux $HOME/.local/bin/fzf-tmux


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

all:
	make init-local-directory
	make install-gitconfig
	make install-tmuxconfig
	make install-vimrc
	make install-zshrc
	make install-nvimrc
	#[[ "$$OSTYPE" == darwin* && $(defaults domains | grep com.googlecode.iterm2) ]] && make load-setting-iterm2


test:
	$(if $(shell [[ "$$OSTYPE" = darwin* ]]), echo 1, echo 2)

# STATEMENT MEMO
# if-statement:
#	 $(if $(command ...),
#	 	@echo 1, \
#	 	@echo 0 \
#	 )
#
#	 <<<"$(<in)" awk '{if(/foo/){x=sub(/foo/, "bar", $0)};print}END{if(x!=1){print "bar"}}' >in

# vim: fdm=marker
