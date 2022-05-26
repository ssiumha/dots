# vim: et ts=2

# time RUN_ZSH_PROFILE=true zsh -i -c exit
[ "$RUN_ZSH_PROFILE" = "true" ] && zmodload zsh/zprof

export LC_CTYPE=en_US.UTF-8
export LC_ALL=en_US.UTF-8
export LANG=en_US.UTF-8

export DOTFILES=${DOTFILES:-$(dirname $0)}

ZSH=$DOTFILES/zsh

default_path=${default_path:-$PATH}
PATH=~/.local/bin:$DOTFILES/bin:$PATH
#default_fpath=${default_fpath:-$FPATH}
#FPATH=$ZSH/functions:$default_fpath

#--------------------------------
# ZI
#--------------------------------
zi_home="${HOME}/.local/zsh/zi"

declare -A ZI
ZI[HOME_DIR]="${zi_home}"
ZI[BIN_DIR]="${zi_home}/bin"
ZI[ZMODULES_DIR]="${zi_home}/zmodules"
ZI[ZCOMPDUMP_PATH]="${HOME}/.local/zcompdump"

if [[ -s "${zi_home}/bin/zi.zsh" ]]
then
  source "${zi_home}/bin/zi.zsh"
  autoload -Uz _zi
  (( ${+_comps} )) && _comps[zi]=_zi

  # @see https://z.digitalclouds.dev/docs/gallery/collection/
  zi light zsh-users/zsh-autosuggestions
  zi light zsh-users/zsh-syntax-highlighting #zi light z-shell/F-Sy-H

  # TODO
  # zi lucid light-mode for pick"z.sh" z-shell/z
  #
  # TODO
  # exa, fd, delta

  ### fzf
  zi wait lucid light-mode for \
    from'gh-r' nocompile atclone'rm -f ~/.local/bin/fzf; ln -s $(pwd)/fzf ~/.local/bin/fzf' \
    junegunn/fzf

  zi ice wait lucid has'fzf'
  zi snippet 'https://github.com/junegunn/fzf/blob/master/shell/completion.zsh'

  ### nvim
  zi ice wait lucid from'gh-r' nocompile \
    bpick"$([[ "$OSTYPE" == darwin* ]] && echo "*macos*" || echo "*linux*tar*")" \
    mv'nvim-* -> nvim' \
    atclone'rm -f ~/.local/bin/nvim; ln -s $(pwd)/nvim/bin/nvim ~/.local/bin/nvim' \
    ver'stable'
  zi light neovim/neovim

  ### ripgrep
  zi ice wait lucid from'gh-r' nocompile \
    mv'ripgrep-* -> ripgrep' atclone'rm -f ~/.local/bin/rg; ln -s $(pwd)/ripgrep/rg ~/.local/bin/rg'
  zi light BurntSushi/ripgrep
else
  command -v git &>/dev/null \
    && git clone https://github.com/z-shell/zi.git "${zi_home}/bin"
fi

#--------------------------------
# Tool Configs
#--------------------------------
export ZSH_AUTOSUGGEST_HIGHLIGHT_STYLE="fg=7"

export MOSH_ESCAPE_KEY='~'

export XDG_CONFIG_HOME="$HOME/.config"

#export VOLTPATH="$DOTFILES/local/vim/volt" # deprecated
export ASDF_CONFIG_FILE="$DOTFILES/asdf/asdfrc"

export RIPGREP_CONFIG_PATH="$XDG_CONFIG_HOME/ripgrep/ripgreprc"
export K9SCONFIG="$XDG_CONFIG_HOME/k9s"

#--------------------------------
# Import
#--------------------------------

[ -f "$HOME/.cargo/env" ] && source "$HOME/.cargo/env"

[ -d "$HOME/.asdf" ] && source "$HOME/.asdf/asdf.sh"

source "$ZSH/common.zsh"
source "$ZSH/completion.zsh"
source "$ZSH/key-binding.zsh"
source "$ZSH/alias.zsh"
source "$ZSH/fzf.zsh"
source "$ZSH/kube.zsh"
source "$ZSH/prompt.zsh"
source "$ZSH/color.zsh"

#--------------------------------
# Functions
#--------------------------------
tmux-version-check() {
  [[ $(echo "$(tmux -V | awk '{print $2}') > $1" | bc) != 0 ]]
}

git-echo-branch-tmux-current-pane() {
  cd `tmux display-message -p -F "#{pane_current_path}"`
  branch_name=`git branch | grep \*.* | sed -e 's/\*\ //'`

  [ ! -z ${branch_name} ] && echo "[${branch_name}]"
}

git-echo-username-and-email() {
  cd `tmux display-message -p -F "#{pane_current_path}"`
  echo "[$(git config --get user.name) | $(git config --get user.email)]"
}

zsh_stats() {
  fc -l 1 | awk '{CMD[$2]++;count++;}END { for (a in CMD)print CMD[a] " " CMD[a]/count*100 "% " a;}'  \
    | grep -v "./" | column -c3 -s " " -t | sort -nr | nl |  head -n20
}

[ "$RUN_ZSH_PROFILE" = "true" ] && zprof

#--------------------------------
# Launcher
#--------------------------------

if [ ! -n "$TMUX" ] && [ "$__IN_DOCKER" = "true" ] && [ -x "$(command -v tmux)" ];
then
  exec tmux
fi
