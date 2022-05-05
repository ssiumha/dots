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
# Zinit
#--------------------------------
ZINIT_HOME="$HOME/.local/zsh/zinit"

declare -A ZINIT
ZINIT[HOME_DIR]="$ZINIT_HOME"
ZINIT[BIN_DIR]="$ZINIT_HOME/bin"
ZINIT[ZCOMPDUMP_PATH]="$HOME/.local/zcompdump"

# not working in zsh 5.3
# declare -A ZINIT=( \
#   ["HOME_DIR"]="$ZINIT_HOME" \
#   ["BIN_DIR"]="$ZINIT_HOME/bin" \
#   ["ZCOMPDUMP_PATH"]="$HOME/.local/zcompdump" \
# )

if [[ -s "$ZINIT_HOME/bin/zinit.zsh" ]] && source "$ZINIT_HOME/bin/zinit.zsh"; then
  autoload -Uz _zinit
  (( ${+_comps} )) && _comps[zinit]=_zinit

  zinit light zsh-users/zsh-autosuggestions
  zinit light zdharma-continuum/fast-syntax-highlighting
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
