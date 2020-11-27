# vim: et ts=2
export LC_CTYPE=en_US.UTF-8
export LC_ALL=en_US.UTF-8
export LANG=en_US.UTF-8

export DOTFILES=${DOTFILES:-$(dirname $0)}

ZSH=$DOTFILES/zsh

default_path=${default_path:-$PATH}
PATH=~/.local/bin:$DOTFILES/bin:$PATH

# ZSH CONFIG

#default_fpath=${default_fpath:-$FPATH}
#FPATH=$ZSH/functions:$default_fpath # lower case fpath is array

autoload -Uz compaudit && compaudit | xargs chmod g-w
autoload -Uz compinit && compinit -C -d "$HOME/.local/zcompdump"

source $ZSH/completion.zsh

# ZPLUG
export ZPLUG_HOME="${ZPLUG_HOME:-$HOME/.local/zsh/zplug}"
if [[ -s "$ZPLUG_HOME/init.zsh" ]] && source "$ZPLUG_HOME/init.zsh"; then
  zplug "zsh-users/zsh-syntax-highlighting", defer:3

  zplug "zsh-users/zsh-autosuggestions"

  if ! zplug check --verbose; then
    printf 'Install? [y/N]:' && read -q && echo && zplug install
  fi

  zplug load
fi

export ZSH_AUTOSUGGEST_HIGHLIGHT_STYLE="fg=7"

# ENABLE META KEY
set meta-flag on
set input-meta on
set convert-meta off
set output-meta on


# EXPORT CONFIG {{{
export TERM=xterm-256color

export EDITOR="vim"

export PAGER="less"

# brew install lesspipe
#test lesspipe.sh && export LESSOPEN="|lesspipe.sh %s" LESS_ADVANCED_PREPROCESSOR=1
export LESS="-R -x4 -i -W -M"

export GREP_COLOR="1;32"
alias grep="grep --color=auto --exclude-dir={.git,.hg,.svn}"

export TIME_STYLE=long-iso

export MOSH_ESCAPE_KEY='~'

export VOLTPATH="$DOTFILES/local/vim/volt"

export LS_COLORS="di=0;34:fi=0:ln=31:pi=5:so=5:bd=5:cd=5:or=31:mi=0:ex=35:ow=0:*.rpm=90"

# BSD COLOR SETTING
export CLICOLOR=1
export LSCOLORS="Gxfxcxdxbxegedabagacad"

if [[ "$OSTYPE" != darwin* || $(which ls) != /bin/ls ]]; then
  export LS_OPTIONS="--color=tty"
fi
alias ls="ls $LS_OPTIONS"

if [[ $OSTYPE == msys* ]]; then
  alias ipconfig="winpty ipconfig"
  alias nslookup="winpty nslookup"
  alias ping="winpty ping"
fi

# }}}


# SETOPT
setopt multios
setopt prompt_subst
setopt long_list_jobs
setopt auto_param_keys

setopt autocd
#cdpath=(~ ~/proj) # cd if not found directory when find at this path

# LIBRARY {{{
# reference: https://github.com/robbyrussell/oh-my-zsh

# directories {{{
setopt auto_pushd
setopt pushd_ignore_dups
setopt pushdminus

alias -g ...='../..'
alias -g ....='../../..'
alias -g .....='../../../..'
alias -g ......='../../../../..'

alias -- -='cd -'
alias d='dirs -v | head -10'
# }}}

# functions {{{
function zsh_stats() {
  fc -l 1 | awk '{CMD[$2]++;count++;}END { for (a in CMD)print CMD[a] " " CMD[a]/count*100 "% " a;}' | grep -v "./" | column -c3 -s " " -t | sort -nr | nl |  head -n20
}

function open_command() {
  emulate -L zsh
  setopt shwordsplit

  local open_cmd

  # define the open command
  case "$OSTYPE" in
    darwin*)  open_cmd='open' ;;
    cygwin*)  open_cmd='cygstart' ;;
    linux*)   open_cmd='xdg-open' ;;
    msys*)    open_cmd='start ""' ;;
    *)        echo "Platform $OSTYPE not supported"
              return 1
              ;;
  esac

  # don't use nohup on OSX
  if [[ "$OSTYPE" == darwin* ]]; then
    $open_cmd "$@" &>/dev/null
  else
    nohup $open_cmd "$@" &>/dev/null
  fi
}
# }}}

# history {{{
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
# }}}

# key-bindings {{{

# http://zsh.sourceforge.net/Doc/Release/Zsh-Line-Editor.html
# http://zsh.sourceforge.net/Doc/Release/Zsh-Line-Editor.html#Zle-Builtins
# http://zsh.sourceforge.net/Doc/Release/Zsh-Line-Editor.html#Standard-Widgets

# Make sure that the terminal is in application mode when zle is active, since
# only then values from $terminfo are valid
if (( ${+terminfo[smkx]} )) && (( ${+terminfo[rmkx]} )); then
  function zle-line-init() {
    echoti smkx
  }
  function zle-line-finish() {
    echoti rmkx
  }
  zle -N zle-line-init
  zle -N zle-line-finish
fi

bindkey -e                                            # Use emacs key bindings
bindkey '\ew' kill-region                             # [Esc-w] - Kill from the cursor to the mark
bindkey '^r' history-incremental-search-backward      # [Ctrl-r] - Search backward incrementally for a specified string. The string may begin with ^ to anchor the search to the beginning of the line.

# start typing + [Up-Arrow] - fuzzy find history forward
if [[ "${terminfo[kcuu1]}" != "" ]]; then
  autoload -U up-line-or-beginning-search
  zle -N up-line-or-beginning-search
  bindkey "${terminfo[kcuu1]}" up-line-or-beginning-search
fi
# start typing + [Down-Arrow] - fuzzy find history backward
if [[ "${terminfo[kcud1]}" != "" ]]; then
  autoload -U down-line-or-beginning-search
  zle -N down-line-or-beginning-search
  bindkey "${terminfo[kcud1]}" down-line-or-beginning-search
fi

if [[ "${terminfo[khome]}" != "" ]]; then
  bindkey "${terminfo[khome]}" beginning-of-line      # [Home] - Go to beginning of line
fi
if [[ "${terminfo[kend]}" != "" ]]; then
  bindkey "${terminfo[kend]}"  end-of-line            # [End] - Go to end of line
fi

if [[ "${terminfo[kcbt]}" != "" ]]; then
  bindkey "${terminfo[kcbt]}" reverse-menu-complete   # [Shift-Tab] - move through the completion menu backwards
fi

# Edit the current command line in $EDITOR
autoload -U edit-command-line
zle -N edit-command-line
bindkey '\C-x\C-e' edit-command-line
# }}}

# }}}

# FUNCTIONS {{{

## IMPORT
source "$ZSH/alias.zsh"
source "$ZSH/fzf.zsh"

__cd_up() { builtin pushd .. > /dev/null; zle accept-line }
zle -N __cd_up
bindkey "^[i" __cd_up

__cd_down() { builtin popd > /dev/null && zle accept-line }
zle -N __cd_down
bindkey "^[o" __cd_down

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
# }}}

source $ZSH/prompt.zsh
source $ZSH/color.zsh

# SSH AGENT {{{
# ref: http://rabexc.org/posts/pitfalls-of-ssh-agents
attach_agent() {
  timeout 3 ssh-add -l &>/dev/null
  if [[ "$?" == 2 ]]; then
    test -r ~/.ssh-agent && \
      eval "$(<~/.ssh-agent)" >/dev/null

    ssh-add -l &>/dev/null
    if [[ "$?" == 2 ]]; then
      (umask 066; ssh-agent > ~/.ssh-agent)
      eval "$(<~/.ssh-agent)" >/dev/null
      ssh-add
    fi
  elif [[ "$?" == 124 ]]; then
    echo "failed ssh-add"
  fi
}
# }}}
