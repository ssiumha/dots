# vim: et ts=2

# time RUN_ZSH_PROFILE=true zsh -i -c exit
[ "$RUN_ZSH_PROFILE" = "true" ] && zmodload zsh/zprof

export LC_CTYPE=en_US.UTF-8
export LC_ALL=en_US.UTF-8
export LANG=en_US.UTF-8

export DOTFILES=${DOTFILES:-$(dirname $0)}

ZSH=$DOTFILES/zsh

default_path=${default_path:-$PATH}

if [ ! $__MY_PATH_INITED ]; then
  PATH=~/.local/bin:$DOTFILES/bin:$PATH
fi

#default_fpath=${default_fpath:-$FPATH}
#FPATH=$ZSH/functions:$default_fpath

export HISTFILE="$HOME/.local/zsh/history"

export XDG_DATA_HOME="$HOME/.local/share"
export XDG_CONFIG_HOME="$HOME/.config"
export XDG_STATE_HOME="$HOME/.local/state"
export XDG_CACHE_HOME="$HOME/.cache"

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

  zi snippet OMZ::lib/key-bindings.zsh
  zi snippet OMZ::lib/completion.zsh
  zi snippet OMZ::lib/clipboard.zsh
  zi snippet OMZ::lib/history.zsh

  zi ice wait lucid as'completion'
  zi snippet 'https://github.com/asdf-vm/asdf/blob/master/completions/_asdf'

  # @see https://z.digitalclouds.dev/docs/gallery/collection/
  zi light zsh-users/zsh-autosuggestions
  zi light zsh-users/zsh-syntax-highlighting #zi light z-shell/F-Sy-H

  __install_local_bin() {
    local name=$1
    local binpath=$2
    local manpath=$3 # TODO

    rm -f "$HOME/.local/bin/$name"
    ln -s "$PWD/$binpath" "$HOME/.local/bin/$name"
  }

  ### fzf
  zi wait lucid light-mode for \
    from'gh-r' nocompile atclone'__install_local_bin fzf fzf' \
    junegunn/fzf

  zi ice wait lucid has'fzf'
  zi snippet 'https://github.com/junegunn/fzf/blob/master/shell/completion.zsh'

  ### nvim
  zi ice wait lucid from'gh-r' nocompile \
    bpick"$([[ "$OSTYPE" == darwin* ]] && echo "*macos*" || echo "*linux*tar*")" \
    mv'nvim-* -> nvim' atclone'__install_local_bin nvim nvim/bin/nvim' \
    ver'stable'
  zi light neovim/neovim

  ### ripgrep
  zi ice wait lucid from'gh-r' nocompile \
    mv'ripgrep-* -> ripgrep' atclone'__install_local_bin rg ripgrep/rg'
  zi light BurntSushi/ripgrep

  ## exa
  zi ice wait lucid from'gh-r' nocompile atclone'__install_local_bin exa bin/exa'
  zi light ogham/exa

  ## delta
  zi ice wait lucid from'gh-r' nocompile \
    mv'delta-* -> delta' atclone'__install_local_bin delta delta/delta'
  zi light dandavison/delta

  ## zoxide
  zi ice wait lucid from'gh-r' nocompile atclone'__install_local_bin zoxide zoxide'
  zi light ajeetdsouza/zoxide

  unset -f __install_local_bin
else
  command -v git &>/dev/null \
    && git clone https://github.com/z-shell/zi.git "${zi_home}/bin"
fi

#--------------------------------
# Tool Configs
#--------------------------------
export ZSH_AUTOSUGGEST_HIGHLIGHT_STYLE="fg=7"

export MOSH_ESCAPE_KEY='~'

#export VOLTPATH="$DOTFILES/local/vim/volt" # deprecated
export ASDF_CONFIG_FILE="$DOTFILES/asdf/asdfrc"

export RIPGREP_CONFIG_PATH="$XDG_CONFIG_HOME/ripgrep/ripgreprc"
export K9SCONFIG="$XDG_CONFIG_HOME/k9s"

#--------------------------------
# Import
#--------------------------------

[ -f "$HOME/.cargo/env" ] && source "$HOME/.cargo/env"

[ -d "$HOME/.asdf" ] && source "$HOME/.asdf/asdf.sh"

if command -v zoxide &>/dev/null; then
  export _ZO_DATA_DIR="$HOME/.local/zsh/zoxide"
  eval "$(zoxide init zsh --no-cmd)"
  alias z=__zoxide_z
  alias zz=__zoxide_zi
fi

source "$ZSH/common.zsh"
source "$ZSH/completion.zsh"
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

get_path() {
  echo $PATH | perl -lape 's/:/\n/g'
}

[ "$RUN_ZSH_PROFILE" = "true" ] && zprof

#--------------------------------
# Path Celansing
#--------------------------------

if [ ! $__MY_PATH_INITED ] && command -v perl &>/dev/null; then
  PATH=$(echo $PATH | perl -pe 's/:/\n/g' | perl -lpe '
          if (/asdf/)            { s/^/01 / }
          elsif (/\.local/)      { s/^/02 / }
          elsif (/.dotfiles/)    { s/^/03 / }
          elsif (/$ENV{"HOME"}/) { s/^/04 / }
          else { s/^/99 / }
        ' | sort -s -n -k 1,1 | awk '!u[$0]++ {print $2}' | tr '\n' ':' | sed 's/:$//')
fi

export __MY_PATH_INITED=true

#--------------------------------
# Launcher
#--------------------------------

if [ ! -n "$TMUX" ] && [ "$__IN_DOCKER" = "true" ] && [ -x "$(command -v tmux)" ];
then
  exec tmux
fi
