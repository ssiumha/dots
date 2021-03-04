#--------------------------------
# Enable Meta Key
#--------------------------------
set meta-flag on
set input-meta on
set convert-meta off
set output-meta on

#--------------------------------
# Setopt
#--------------------------------
setopt multios
setopt prompt_subst
setopt long_list_jobs
setopt auto_param_keys

setopt autocd
#cdpath=(~ ~/proj) # can setting global search directory

#--------------------------------
# Export
#--------------------------------
export TERM=xterm-256color
export EDITOR="vim"
export PAGER="less"
export LESS="-R -x4 -i -W -M"
export TIME_STYLE=long-iso

export GREP_COLOR="1;32"
alias grep="grep --color=auto --exclude-dir={.git,.hg,.svn}"

## ls config
export LS_COLORS="di=0;34:fi=0:ln=31:pi=5:so=5:bd=5:cd=5:or=31:mi=0:ex=35:ow=0:*.rpm=90"

## BSD COLOR SETTING
export CLICOLOR=1
export LSCOLORS="Gxfxcxdxbxegedabagacad"

if [[ "$OSTYPE" != darwin* || $(which ls) != /bin/ls ]]; then
  export LS_OPTIONS="--color=tty"
fi
alias ls="ls $LS_OPTIONS"

#--------------------------------
# Directory
#--------------------------------
setopt auto_pushd
setopt pushd_ignore_dups
setopt pushdminus

alias -g ...='../..'
alias -g ....='../../..'
alias -g .....='../../../..'
alias -g ......='../../../../..'

alias -- -='cd -'
alias d='dirs -v | head -10'

#--------------------------------
# History
#--------------------------------
if [ -z "$HISTFILE" ]; then
    HISTFILE=$HOME/.zsh_history
fi

HISTSIZE=10000
SAVEHIST=10000

setopt append_history
setopt extended_history
setopt hist_expire_dups_first
setopt hist_ignore_dups # ignore duplication command history list
setopt hist_ignore_space
setopt hist_verify
setopt inc_append_history
setopt share_history # share command history data
