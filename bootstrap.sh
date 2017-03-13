#!/bin/bash
[[ ! -s "$HOME/.gitconfig" ]] && git config --global include.path "~/dotfiles/gitconfig"
[[ ! -s "$HOME/.tmux.conf" ]] && echo "source-file ~/dotfiles/tmux.conf" > $HOME/.tmux.conf
[[ ! -s "$HOME/.vimrc" ]] && echo "source ~/dotfiles/vimrc" > "$HOME/.vimrc"
[[ ! -s "$HOME/.zshrc" ]] && echo "source ~/dotfiles/zshrc" > "$HOME/.zshrc"

mkdir -p "$HOME/.local/"{repo,bin,vim,zsh/completion,zsh/zplug}

#[[ ! -s "$HOME/.local/vim/autoload/plug.vim" ]] &&
#    curl -fLo ~/.local/vim/autoload/plug.vim --create-dirs \
#            https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
[[ ! -d "$HOME/.local/vim/dein.vim" ]] &&
    git clone --depth=1 "https://github.com/Shougo/dein.vim" "$HOME/.local/vim/dein.vim"

[[ ! -s "$HOME/.local/bin/diff-highlight" ]] && \
    curl -fLo "$HOME/.local/bin/diff-highlight" \
        "https://raw.githubusercontent.com/git/git/master/contrib/diff-highlight/diff-highlight"

[[ ! -s "$HOME/.local/bin/ack" ]] && \
    curl -fL http://beyondgrep.com/ack-2.14-single-file > $HOME/.local/bin/ack && chmod 0755 $HOME/.local/bin/ack

[[ ! -s "$HOME/.local/bin/fzy" ]] && \
    git clone "https://github.com/jhawthorn/fzy.git" "$HOME/.local/repo/fzy" && cd "$HOME/.local/repo/fzy" && make && make install -e "PREFIX=$HOME/.local/" && cd -

[[ ! -s "$HOME/.local/zsh/zplug/init.zsh" ]] && \
    printf 'install zplug? [y/N]:' && read -q && echo && \
    ZPLUG_HOME=$HOME/.local/zsh/zplug curl -sL zplug.sh/installer | zsh

