#!/bin/bash
[[ ! -s "$HOME/.gitconfig" ]] && git config --global include.path "~/dotfiles/gitconfig"
[[ ! -s "$HOME/.tmux.conf" ]] && echo "source-file ~/dotfiles/tmux.conf" > $HOME/.tmux.conf
[[ ! -s "$HOME/.vimrc" ]] && echo "source ~/dotfiles/vimrc" > "$HOME/.vimrc"
[[ ! -s "$HOME/.zshrc" ]] && echo "source ~/dotfiles/zshrc" > "$HOME/.zshrc"

mkdir -p "$HOME/.local/"{repo,bin,vim,zsh/completion}

[[ ! -s "$HOME/.local/vim/autoload/plug.vim" ]] &&
    curl -fLo ~/.local/vim/autoload/plug.vim --create-dirs \
            https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim

[[ ! -s "$HOME/.local/bin/diff-highlight" ]] && \
    curl -fLo "$HOME/.local/bin/diff-highlight" \
        "https://raw.githubusercontent.com/git/git/master/contrib/diff-highlight/diff-highlight"

[[ ! -s "$HOME/.local/bin/fzy" ]] && \
    git clone "https://github.com/jhawthorn/fzy.git" "$HOME/.local/repo/fzy" && cd "$HOME/.local/repo/fzy" && make && make install -e "PREFIX=$HOME/.local/" && cd -
