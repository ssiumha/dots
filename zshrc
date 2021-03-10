# vim: et ts=2

# Profiling
# time zsh -i -c exit
#
# zmodload zsh/zprof
# zprof # run at eof

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
declare -A ZINIT=( \
  ["HOME_DIR"]="$ZINIT_HOME" \
  ["BIN_DIR"]="$ZINIT_HOME/bin" \
  ["ZCOMPDUMP_PATH"]="$HOME/.local/zcompdump" \
)

if [[ -s "$ZINIT_HOME/bin/zinit.zsh" ]] && source "$ZINIT_HOME/bin/zinit.zsh"; then
  autoload -Uz _zinit
  (( ${+_comps} )) && _comps[zinit]=_zinit

  zinit light zsh-users/zsh-autosuggestions
  zinit light zdharma/fast-syntax-highlighting
fi

#--------------------------------
# Tool Configs
#--------------------------------
export ZSH_AUTOSUGGEST_HIGHLIGHT_STYLE="fg=7"

export MOSH_ESCAPE_KEY='~'

#export VOLTPATH="$DOTFILES/local/vim/volt" # deprecated
export RIPGREP_CONFIG_PATH="$DOTFILES/ripgreprc"
export ASDF_CONFIG_FILE="$DOTFILES/asdfrc"

#--------------------------------
# Import
#--------------------------------
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

