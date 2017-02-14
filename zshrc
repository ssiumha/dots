# vim: et ts=2
export LC_CTYPE=en_US.UTF-8
export LC_ALL=en_US.UTF-8
export LANG=en_US.UTF-8

ZSH=~/dotfiles/zsh

default_path=${default_path:-$PATH}
PATH=~/.local/bin:~/dotfiles/bin:$PATH

# ZSH CONFIG
autoload -Uz compinit && compinit -C -d "$HOME/.local/zcompdump"
autoload -Uz colors && colors

#default_fpath=${default_fpath:-$FPATH}
#FPATH=$ZSH/functions:$default_fpath # lower case fpath is array

# ZPLUG
# curl -sL zplug.sh/installer | zsh
if [[ -s "$HOME/.zplug/init.zsh" ]] && source $HOME/.zplug/init.zsh; then
  zplug "zsh-users/zsh-syntax-highlighting", nice:10

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
set convert-meta on
set output-meta on


# EXPORT CONFIG {{{
export TERM=xterm-256color

export EDITOR="vim"

export PAGER="less"
export LESS="-R -x4"

export GREP_COLOR="1;32"
alias grep="grep --color=auto --exclude-dir={.git,.hg,.svn}"

export TIME_STYLE=long-iso

export MOSH_ESCAPE_KEY=''

export LS_OPTIONS="--color"
export LS_COLORS="di=0;34:fi=0:ln=31:pi=5:so=5:bd=5:cd=5:or=31:mi=0:ex=35:ow=0:*.rpm=90"
export LSCOLORS="Gxfxcxdxbxegedabagacad"

ls --color -d . &>/dev/null 2>&1 &&
  alias ls='ls --color=tty' || alias ls='ls -G'

#alias history='fc -il 1'
alias history='fc -l 1'

if [[ $OSTYPE == msys* ]]; then
  alias ipconfig="winpty ipconfig"
  alias nslookup="winpty nslookup"
  alias ping="winpty ping"
fi

# }}}

# ALIAS {{{
alias ll="ls -lAh"
alias l="ls -lh"

l_func() {
  ls -lh --color=always $@ | awk '
  $1 ~ /^[[:alpha:]-]{10}$/{ a=1; }
  !a{ print }
  a{
    if(!/^d/){ d = d$0"\n"; }
    else{ print }
  }
  /^$/{ printf d; a=0; d=""; }
  END{ printf d; }
  '
}
alias l=l_func

alias g="git"
alias v="vim"

# suffix
alias -s py=python
alias -s html=open
function extract() {
  case $1 in
    *.tar.gz|*.tgz) tar xzvf $1;;
    *.tar.xz) tar Jxvf $1;;
    *.zip) unzip $1;;
    *.lzh) lha e $1;;
    *.tar.bz2|*.tbz) tar xjvf $1;;
    *.tar.Z) tar zxvf $1;;
    *.gz) gzip -d $1;;
    *.bz2) bzip2 -dc $1;;
    *.Z) uncompress $1;;
    *.tar) tar xvf $1;;
    *.arj) unarj $1;;
  esac
}
alias -s {gz,tgz,zip,lzh,bz2,tbz,Z,tar,arj,xz}=extract

alias -g gB='$(git branch -a | fzy --prompt "GIT BRANCH>" | sed -e "s/^\*\s*//g")'
alias -g gR='$(git remote -a | fzy --prompt "GIT REMOTE>")'
alias -g gH='$(git log --oneline --branches | fzy --prompt "GIT HASH>" | awk "{print \$1}")'
#alias -g gS='$(git status --short | fzf-tmux +s --multi --ansi --prompt "GIT STATUS>" | sed -e "s/^.. //")'

# 자신의 github repo 목록중에서 선택하기
#alias -g H='`curl -sL https://api.github.com/users/YOUR_USERNAME/repos | jq -r ".[].full_name" | peco --prompt "GITHUB REPOS>" | head -n 1`'

#alias ssh='ssh -F <(find ~/.ssh/ -maxdepth 1 -name "*.config" -or -name config -exec cat {} \+)'

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

# completion {{{
unsetopt menu_complete   # do not autoselect the first completion entry
unsetopt flowcontrol
setopt auto_menu         # show completion menu on succesive tab press
setopt complete_in_word
setopt always_to_end

WORDCHARS=''

zmodload -i zsh/complist

bindkey -M menuselect '^o' accept-and-infer-next-history

## case-insensitive (all),partial-word and then substring completion
#zstyle ':completion:*' matcher-list 'r:|[._-]=* r:|=*' 'l:|=* r:|=*'  # CASE_SENSITIVE
zstyle ':completion:*' matcher-list 'm:{a-zA-Z-_}={A-Za-z_-}' 'r:|[._-]=* r:|=*' 'l:|=* r:|=*'  # HYPHEN_INSENSITIVE
#zstyle ':completion:*' matcher-list 'm:{a-zA-Z}={A-Za-z}' 'r:|[._-]=* r:|=*' 'l:|=* r:|=*'

zstyle '*' single-ignored show
zstyle ':completion:*:*:*:*:*' menu select

zstyle ':completion:*' list-colors ${(s.:.)LS_COLORS}
zstyle ':completion:*:*:kill:*:processes' list-colors '=(#b) #([0-9]#) ([0-9a-z-]#)*=01;34=0=01'

zstyle ':completion:*' list-separator '-->'
zstyle ':completion:*:manuals' separate-sections true

# disable named-directories autocompletion
zstyle ':completion:*:cd:*' tag-order local-directories directory-stack path-directories

# Use caching so that commands like apt and dpkg complete are useable
zstyle ':completion::complete:*' use-cache 1
zstyle ':completion::complete:*' cache-path $ZSH_CACHE_DIR

# Don't complete uninteresting users
zstyle ':completion:*:*:*:users' ignored-patterns \
        adm amanda apache at avahi avahi-autoipd beaglidx bin cacti canna \
        clamav daemon dbus distcache dnsmasq dovecot fax ftp games gdm \
        gkrellmd gopher hacluster haldaemon halt hsqldb ident junkbust kdm \
        ldap lp mail mailman mailnull man messagebus  mldonkey mysql nagios \
        named netdump news nfsnobody nobody nscd ntp nut nx obsrun openvpn \
        operator pcap polkitd postfix postgres privoxy pulse pvm quagga radvd \
        rpc rpcuser rpm rtkit scard shutdown squid sshd statd svn sync tftp \
        usbmux uucp vcsa wwwrun xfs '_*'

if [ "$OSTYPE[0,7]" = "solaris" ]
then
  zstyle ':completion:*:*:*:*:processes' command "ps -u $USER -o pid,user,comm"
else
  zstyle ':completion:*:*:*:*:processes' command "ps -u $USER -o pid,user,comm -w -w"
fi

# completion waiting dots
expand-or-complete-with-dots() {
  # toggle line-wrapping off and back on again
  [[ -n "$terminfo[rmam]" && -n "$terminfo[smam]" ]] && echoti rmam
  print -Pn "%{%F{red}......%f%}"
  [[ -n "$terminfo[rmam]" && -n "$terminfo[smam]" ]] && echoti smam

  zle expand-or-complete
  zle redisplay
}
zle -N expand-or-complete-with-dots
bindkey "^I" expand-or-complete-with-dots

# }}}

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
# fshow - git commit browser (enter for show, ctrl-d for diff)
fshow() {
  local out shas sha q k
  while out=$(
      git log --graph --color=always \
          --format="%C(auto)%h%d %s %C(black)%C(bold)%cr" "$@" |
      fzf --ansi --multi --no-sort --reverse --query="$q" \
          --print-query --expect=ctrl-d); do
    q=$(head -1 <<< "$out")
    k=$(head -2 <<< "$out" | tail -1)
    shas=$(sed '1,2d;s/^[^a-z0-9]*//;/^$/d' <<< "$out" | awk '{print $1}')
    [ -z "$shas" ] && continue
    if [ "$k" = ctrl-d ]; then
      git diff --color=always $shas | less -R
    else
      for sha in $shas; do
        git show --color=always $sha | less -R
      done
    fi
  done
}

if type fzy &>/dev/null;
then
  fzy-select-history() {
    BUFFER="$(history | tac |
      perl -pe 's/^\s*\d+\*?\s+//' |
      awk '!a[$0]++' |
      fzy --query "$LBUFFER" | # peco --query "$LBUFFER"
      sed 's/\\n/\n/')"
    CURSOR=$#BUFFER             # cursor move to line end
    zle reset-prompt
  }
  zle -N fzy-select-history     # register widget
  bindkey '^R' fzy-select-history

  fzy-favorite() {
    BUFFER="$(cat $ZSH/favorite.txt | fzy --query "$LBUFFER" | sed 's/\\n/\n/')"
    CURSOR=$#BUFFER
    zle reset-prompt
  }
  zle -N fzy-favorite
  bindkey '^F' fzy-favorite

  fzy-ssh() {
    ssh $@ "$(cat ~/.ssh/config | grep ^Host | awk '{print $2}' |
      fzy --query "$LBUFFER" | sed 's/\\n/\n/')"
  }
  alias -g s='fzy-ssh'

  fzy-move-path() {
    #if git rev-parse 2> /dev/null; then
    #  source_files=$(git ls-files)
    #else
    #  source_files=$(find . -type f)
    #fi
    cd "$(find . -type d | egrep -v '(.git|.svn)' |
      awk 'NR>1' | fzy --query "$LBUFFER")"
  }
  alias -g c='fzy-move-path'
fi

mru() {
  cd "$(cat ~/.zsh_cdhistory 2>/dev/null |
    egrep -v '(.git|.svn)' | sort -u | fzy --query "$LBUFFER")"
}

##zle accept-line # run current buffer == cr

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

# PROMPT {{{
git_dirty() {
  rev-parse --is-inside-work-tree &>/dev/null || return
  diff --quiet --ignore-submodules HEAD &>/dev/null; [ $? -eq 1 ] && echo "*"
}

repo_type() {
  git branch &>/dev/null && echo 'git' && return
  echo ''
}

battery_charge() {
  echo $(ioreg -rc AppleSmartBattery 2>/dev/null |
    awk '/CurrentCap/{a=$3}
      /MaxCap/{b=$3}
      END{printf("%.1f%%", a/b*100)}
    ' 2>/dev/null)
}

venv_info() {
  [ $VIRTUAL_ENV ] && echo '('`basename $VIRTUAL_ENV`') '
}

cmd_exec_time() {
  [ ${cmd_timestamp:-0} -eq 0 ] && return
  let local elapsed=$(date +%s)-$cmd_timestamp
  [ $elapsed -gt 3 ] && echo "${elapsed}s"
}

git_repo_info() {
  git status --short --branch --untracked-files=no 2>/dev/null | \
    gawk '
      NR==1{printf " "gensub(/^(.*)\.{3}.+$/, "\\1", "g", $2)}
      !a&&/^A/{a="+"}
      !m&&/^.M/{m="!"}
      END{ if(NR){printf a""m" "} }
    '
}

chpwd() {
  [ "`pwd`" != ~ ] && pwd >> ~/.zsh_cdhistory && sed -n '1,10000p' -i ~/.zsh_cdhistory
}

preexec() {
  cmd_timestamp=`date +%s`
}

precmd() {
  #[%D{%y-%d-%m %H:%M}]
  # TODO : %~ coloring. symbolic:cyan(6), current:bold?
  # %F-fg, %K-bg, %S-reverse
  txt="\n"
  if [ ! -n "$TMUX" ]; then
    txt+="%K{8} %n@%m %K{0}"
  fi
  txt+="%K{7} %~ %K{0}"
  txt+="%K{8}$(git_repo_info)%K{0}"
  txt+=" %F{11}$(cmd_exec_time)"
  print -P $txt
  cmd_timestamp=0
}

rprompt_func() {
  # RPROMPT="%F{8}${SSH_TTY:+%n@%m}%f"
  txt=""
  [[ ! -z $PYENV_VERSION ]] && txt+="%F{7}venv:${PYENV_VERSION}%F{0}"

  echo -e $txt
}


PROMPT='%(?.%F{13}.%F{1})❯%f '
RPROMPT='$(rprompt_func)'
PS1=$PROMPT

# }}}

# COLOR {{{

# '\e[x;ym $TEXT \e[m'
# x;y;.. is color pair
# 0:ResetAll, 1:Bold, 2:Dim, 4:Underline, 5:Blink, 7:Invert, 8:Hidden
# 21:ResetBold, 22:RestDim, 24:Reset UL, 25: Reset Blink ...
# 30 ~ 37, 90~97 : Foreground
# 39 : Default Foreground
# 40 ~ 47, 100~107 : Background
# 49 : Default Foreground
# 38;5;colcode or 48;5;colcode : use 255 colors
# 90~97 == 1;30~1;37
#
# or
# echo $(tput setaf 4) TEXT
# tput [ bold | rev | sgr0 | setaf $colcode | setab $colcode ]
#
# pirnt current rgb: printf "\033Ptmux;\033\033]4;9;?\007\033\\"

if [ -n "$TMUX" ]; then
  # tell tmux to pass the escape sequences through
  # (Source: http://permalink.gmane.org/gmane.comp.terminal-emulators.tmux.user/1324)
  printf_template="\033Ptmux;\033\033]4;%d;rgb:%s\007\033\\"
  printf_template_var="\033Ptmux;\033\033]%d;rgb:%s\007\033\\"
elif [ "${TERM%%-*}" = "screen" ]; then
  # GNU screen (screen, screen-256color, screen-256color-bce)
  printf_template="\033P\033]4;%d;rgb:%s\007\033\\"
  printf_template_var="\033P\033]%d;rgb:%s\007\033\\"
else
  printf_template="\033]4;%d;rgb:%s\033\\"
  printf_template_var="\033]%d;rgb:%s\033\\"
fi

printf $printf_template 0  "1d/1f/21" # K 0;30
printf $printf_template 1  "cb/76/76" # R 0;31     Drak Red
printf $printf_template 2  "63/b4/63" # G 0;32     ...
printf $printf_template 3  "ce/95/5b" # Y 0;33
printf $printf_template 4  "68/91/ba" # B 0;34
printf $printf_template 5  "9d/81/c6" # M 0;35
printf $printf_template 6  "52/ba/ba" # C 0;36
printf $printf_template 7  "73/76/80" # W 0;37     Light Gray
printf $printf_template 8  "50/50/59" # BK 1;30    Drak Gray
printf $printf_template 9  "df/a1/ba" # BR 1;31    Light Red
printf $printf_template 10 "ab/d2/84" # BG 1;32    ...
printf $printf_template 11 "db/db/70" # BY 1;33
printf $printf_template 12 "95/ac/da" # BB 1;34
printf $printf_template 13 "bc/7f/bc" # BM 1;35
printf $printf_template 14 "7e/c4/a0" # BC 1;36
printf $printf_template 15 "e1/e1/e1" # BW 1;37    White

printf $printf_template_var 10 "e1/e1/e1" # color foreground
printf $printf_template_var 11 "1d/1f/21" # color background
printf $printf_template_var 12 "e1/e1/e1" # color cursor

unset printf_template
unset printf_template_var

color_test() {
  #for (( j=0; j<16; j++ )); do
  for j in {0,8,7,15,1,9,2,10,3,11,4,12,5,13,6,14}; do
    for (( i=0; i<16; i++ )); do
      #printf "\e[48;5;${i}m%03d" $i;
      printf "\e[48;5;${j};38;5;${i}m%02d:Txt" $i;
      printf '\e[0m';
      [ ! $((($i - 15) % 8)) -eq 0 ] && printf '' || printf '\n'
    done
  done
}
# }}}

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
