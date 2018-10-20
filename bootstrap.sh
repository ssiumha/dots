#!/bin/bash
DOTFILES=${DOTFILES:-$HOME/dotfiles}

[[ ! -s "$HOME/.gitconfig" ]] && git config --global include.path "$DOTFILES/gitconfig"
[[ ! -s "$HOME/.tmux.conf" ]] && echo "source-file $DOTFILES/tmux.conf" > $HOME/.tmux.conf
[[ ! -s "$HOME/.vimrc" ]] && echo "source $DOTFILES/vimrc" > "$HOME/.vimrc"
[[ ! -s "$HOME/.zshrc" ]] && echo "source $DOTFILES/zshrc" > "$HOME/.zshrc"

mkdir -p "$HOME/.local/"{repo,bin,vim,zsh/completion,zsh/zplug,emacs/tmp}
mkdir -p "$HOME/.local/vim/tmp/"{undo,backup,sawp}


_check_y() {
    local key_press="";
    read -rsn 1 key_press;
    printf "\n";
    return $([[ $key_press == y ]]);
}


# iterm2 setting
[[ "$OSTYPE" == darwin* && $(defaults domains | grep com.googlecode.iterm2) ]] && \
    defaults write com.googlecode.iterm2.plist PrefsCustomFolder -string "$DOTFILES/iterm2" && \
    defaults write com.googlecode.iterm2.plist LoadPrefsFromCustomFolder -bool true


#[[ ! -s "$HOME/.local/vim/autoload/plug.vim" ]] &&
#    curl -fLo ~/.local/vim/autoload/plug.vim --create-dirs \
#            https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim


[[ ! -s "$HOME/.local/bin/diff-highlight" ]] && \
    (echo -e '#!/usr/bin/env perl\n'; \
     curl https://raw.githubusercontent.com/git/git/master/contrib/diff-highlight/{DiffHighlight.pm,diff-highlight.perl}) > "$HOME/.local/bin/diff-highlight" && \
     chmod +x "$HOME/.local/bin/diff-highlight"



[[ ! -s "$HOME/.local/bin/ack" ]] && \
    curl -fL http://beyondgrep.com/ack-2.14-single-file > $HOME/.local/bin/ack && chmod 0755 $HOME/.local/bin/ack

[[ ! -s "$HOME/.local/bin/fzy" ]] && \
    git clone --depth=1 "https://github.com/jhawthorn/fzy.git" "$HOME/.local/repo/fzy" && \
        cd "$HOME/.local/repo/fzy" && make && make install -e "PREFIX=$HOME/.local/" && cd -

[[ ! -s "$HOME/.local/zsh/zplug/init.zsh" ]] && \
    printf 'install zplug? [y/N]:' && _check_y && \
    export ZPLUG_HOME=$HOME/.local/zsh/zplug && \
    git clone https://github.com/zplug/zplug $ZPLUG_HOME && source $ZPLUG_HOME/init.zsh


[[ ! -s "$HOME/.local/bin/volt" ]] && \
    printf 'install volt...' && \
        curl -s https://api.github.com/repos/vim-volt/volt/releases/latest \
        | grep browser_download_url \
        | case "$OSTYPE" in \
            darwin*) grep darwin-amd64 ;;
            *) grep linux-$( (uname -a | grep 'x86-64' > /dev/null) && echo 'amd64' || echo '386') ;; esac \
        | cut -d '"' -f 4 \
        | wget -i - -O "$HOME/.local/bin/volt" && chmod +x "$HOME/.local/bin/volt"


[[ ! -s "$HOME/.local/bin/fzf" ]] && \
    printf 'install fzf...' && \
        git clone --depth=1 "git@github.com:junegunn/fzf.git" "$HOME/.local/repo/fzf" && \
        $HOME/.local/repo/fzf/install --bin && \
        ln -s $HOME/.local/repo/fzf/bin/fzf $HOME/.local/bin/fzf && \
        ln -s $HOME/.local/repo/fzf/bin/fzf-tmux $HOME/.local/bin/fzf-tmux

unset -f _check_y

return 0
