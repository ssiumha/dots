# vim: et ts=2

# time RUN_ZSH_PROFILE=true zsh -i -c exit
[ "$RUN_ZSH_PROFILE" = "true" ] && export ZSH_DEBUG=true && zmodload zsh/zprof

# TODO: export _ZSH_INIT_MINIMAL= is wsl

################################
# Common
################################
set meta-flag on
set input-meta on
set convert-meta off
set output-meta on

setopt multios            # multi redirection
setopt prompt_subst       # $(), ``
setopt long_list_jobs     # show jobs
setopt auto_param_keys    # auto complete -<tab>
setopt autocd             # omit `cd`

setopt auto_pushd         # works auto pushd when cd
setopt pushd_ignore_dups  # uniq directory stack
setopt pushdminus         # can use -, $OLDPWD, when pushed
setopt no_nomatch         # no error when 'app/(router)/...'

################################
# Path
################################
case $OSTYPE in
  darwin*)
    export path=(
      # <- mise shims
      "$HOME/dots/bin"
      "$HOME/.local/bin"
      "$HOME/.local/share/krew/bin"
      "/opt/homebrew/bin"
      "/opt/homebrew/sbin"
      "/usr/local/bin"
      "/usr/bin"
      "/bin"
      "/usr/sbin"
      "/sbin"
      "/Library/Apple/usr/bin"
      "$HOME/.orbstack/bin"
    )
    ;;
  *)
    echo $PATH | grep -q "$HOME/.local/bin" && : || export PATH="$HOME/.local/bin:$PATH"
    echo $PATH | grep -q "$HOME/dots/bin" && : || export PATH="$HOME/dots/bin:$PATH"
    ;;
esac

################################
# Env
################################
export TIME_STYLE=long-iso

# export ZDOTDIR="$HOME/.local/zsh" # Not Working?

export XDG_DATA_HOME="$HOME/.local/share"
export XDG_CONFIG_HOME="$HOME/.config"
export XDG_STATE_HOME="$HOME/.local/state"
export XDG_CACHE_HOME="$HOME/.cache"

export HISTSIZE=500000
export SAVEHIST=500000

export ZSH_CACHE_DIR="$XDG_CACHE_HOME/zsh/.zcompcache"

export ASDF_CONFIG_FILE="$XDG_CONFIG_HOME/asdf/asdfrc"
export RIPGREP_CONFIG_PATH="$XDG_CONFIG_HOME/ripgrep/ripgreprc"
export K9SCONFIG="$XDG_CONFIG_HOME/k9s"

export DENO_INSTALL_ROOT="$XDG_CACHE_HOME/deno"
export KREW_ROOT="$XDG_DATA_HOME/krew"

[[ "$TERM_PROGRAM" = "vscode" ]] && export EDITOR="code --wait"

#### LS
export LS_COLORS="di=0;34:fi=0:ln=31:pi=5:so=5:bd=5:cd=5:or=31:mi=0:ex=35:ow=0:*.rpm=90"

# BSD
export CLICOLOR=1
export LSCOLORS="Gxfxcxdxbxegedabagacad"

if [[ "$OSTYPE" != darwin* || $(which ls) != /bin/ls ]]; then
  export LS_OPTIONS="--color=tty"
fi
alias ls="ls -A $LS_OPTIONS"

# AWS
export AWS_CLI_AUTO_PROMPT=on-partial
####


################################
# Alias (Basic)
################################
alias mv="mv -i"
alias cp="cp -i"

alias g="git"

alias -g ...=../..
alias -g ....=../../..

alias k="kubectl"
ksw() {
  kubectl config get-contexts \
    | perl -nale 'next if $.==1; s/^(.)\s+([^\s]+).+$/$1 $2/; print' \
    | fzf | cut -c3- | xargs -I% kubectl config use-context %
}

alias j="just"
alias jl="JUST_JUSTFILE=justfile.local just"

alias mux="tmuxinator"

alias ~d="$HOME/dots"
alias ~r="$HOME/room"

function cdr() {
  local git_root=$(git rev-parse --show-toplevel)
  cd "$git_root/$1"
}
_fzf_complete_cdr() {
  local git_root=$(git rev-parse --show-toplevel)
  _fzf_complete --min-height 15 -- "$@" < <(
    fd --color never -td . "$git_root" \
      | ruby -ne "BEGIN {puts '.'}; puts \$_.sub('$git_root/', '').chomp"
  )
}

function zp() {
  # display: @/app
  # move: ~/..
  local git_root=$(git rev-parse --show-toplevel)
  local cd_path=$(
    fd --color never -td . "$git_root" \
      | ruby -ne "BEGIN {puts '.'}; puts \$_.sub('$git_root/', '').chomp" \
      | fzf \
          --scheme=path \
          --preview "lsd --color always --tree --depth 2 $git_root/{+}" \
          --preview-window 'right:40'
  )

  if [ -z "$cd_path" ]; then
    return
  fi

  cd "$git_root/$cd_path"
}

################################
# Alias (Docker)
################################

alias doggo="docker run --rm ghcr.io/mr-karan/doggo:latest"
alias laws="AWS_PROFILE=localstack aws"

################################
# Prompt
################################

mkdir -p /tmp/zsh_prompt/
git_repo_info() {
  if ! git rev-parse --is-inside-work-tree &>/dev/null; then
    return
  fi

  local output_path="/tmp/zsh_prompt/${$}_"
  (git rev-parse --abbrev-ref HEAD) > "${output_path}_branch" &
  (git diff-index --quiet HEAD && echo '' || echo '!') > "${output_path}_changes" &
  (git diff-index --cached --diff-filter=A --quiet HEAD && echo '' || echo '+') > "${output_path}_added" &
  wait

  cat "${output_path}_branch" "${output_path}_changes" "${output_path}_added" 2>/dev/null | tr -d "\n"
}

precmd() {
  #[%D{%y-%d-%m %H:%M}]
  # TODO : %~ coloring. symbolic:cyan(6), current:bold?
  # %F-fg, %K-bg, %S-reverse

  local reset_color="\e[49m\e[39m"

  txt="\n"
  txt+="%K{0} %~ ${reset_color}"
  txt+="%K{8} $(git_repo_info) ${reset_color}"

  print -P $txt
}


# echo -ne "\033]10;#FFFFFF\007" # default text color

PROMPT='%(?.%F{13}.%F{1})>%f '
PS1=$PROMPT


################################
# Import
################################

# TODO: curl https://mise.jdx.dev/install.sh | sh
if [ -d "$HOME/.local/share/mise" ]; then
  eval "$($HOME/.local/bin/mise activate zsh)"
  alias m="mise run"
elif [ -d "$HOME/.asdf" ]; then
  source "$HOME/.asdf/asdf.sh"
fi

if mise ls zoxide &>/dev/null; then
  export _ZO_DATA_DIR="$HOME/.local/zsh/zoxide"
  eval "$(mise x -- zoxide init zsh --no-cmd)"
  alias z=__zoxide_z
  alias zz=__zoxide_zi
fi

################################
# Alias (with mise)
################################

if command -v nvim &>/dev/null || mise ls neovim &>/dev/null; then
  alias v="nvim"
  export EDITOR="nvim"
else
  alias v="vim"
  export EDITOR="vim"
fi

if command -v lsd &>/dev/null || mise ls lsd &>/dev/null; then
  alias l="lsd -Ah"
  alias la="lsd -Ah"
  alias ll="lsd -Alh"
  alias lla="lsd -lAh"
  alias lt="l -Ah --tree"
elif command -v exa &>/dev/null; then
  alias l="exa -s type"
  alias la="exa -s type -a"
  alias ll="exa -s type -l"
  alias llg="exa -s type -l --git"
  alias lla="exa -s type -la"
  alias lt="exa -s type --tree -l"
else
  alias l="ls -Ah"
  alias la="ls -Ah"
  alias ll="ls -lAh"
  alias lla="ls -lAh"
fi

################################
# Zinit
################################
zinit_home="${HOME}/.local/zsh/zinit"

declare -A ZINIT
ZINIT[HOME_DIR]="${zinit_home}"
ZINIT[BIN_DIR]="${zinit_home}/bin"
ZINIT[ZMODULES_DIR]="${zinit_home}/zmodules"
ZINIT[ZCOMPDUMP_PATH]="${HOME}/.local/zcompdump"

if [[ -s "${zinit_home}/bin/zinit.zsh" ]]
then
  source "${zinit_home}/bin/zinit.zsh"

  if [[ "$_ZSH_INIT_MINIMAL" != true ]]; then
    autoload -Uz _zinit
    (( ${+_comps} )) && _comps[zinit]=_zinit
  fi

  # https://github.com/ohmyzsh/ohmyzsh/blob/master/lib
  zinit snippet OMZ::lib/clipboard.zsh
  zinit snippet OMZ::lib/completion.zsh
  zinit snippet OMZ::lib/history.zsh
  zinit snippet OMZ::lib/key-bindings.zsh

  zinit light zsh-users/zsh-autosuggestions
  zinit light zsh-users/zsh-completions

  zinit ice wait"0" silent
  zinit light zsh-users/zsh-syntax-highlighting

  zinit ice wait"2" silent
  zinit light chitoku-k/fzf-zsh-completions

  zinit ice from"gh" as"program" pick"bin/*"
  zinit light reegnz/jq-zsh-plugin

  # zinit snippet 'https://github.com/asdf-vm/asdf/blob/master/completions/_asdf'
  # zinit snippet 'https://github.com/junegunn/fzf/blob/master/shell/completion.zsh'
  zinit snippet 'https://github.com/git/git/blob/master/contrib/completion/git-prompt.sh'
else
  command -v git &>/dev/null \
    && git clone --depth=5 https://github.com/zdharma-continuum/zinit "${zinit_home}/bin"
fi

################################
# After Zinit
################################
export WORDCHARS="-/_@" # for word movement. Empty in completion.zsh

################################
# completion
################################

if [[ "$_ZSH_INIT_MINIMAL" != true ]]; then
  export ZSH_COMPDUMP=~/.cache/zcompdump

  autoload -Uz compinit
  if [ ! -f "$ZSH_COMPDUMP" ] || [ $(find "$ZSH_COMPDUMP" -mtime +1 -print) ]; then
    echo 'compinit!'
    rm -f "$ZSH_COMPDUMP"

    compinit

    mise which aws_completer &>/dev/null && complete -C $(mise which aws_completer) aws
  fi

  compinit -C -d "$ZSH_COMPDUMP"

  command -v mise &>/dev/null && eval "$(mise completion zsh)"
  mise which just &>/dev/null && eval "$(mise x -- just --completions zsh)"
  mise which fzf &>/dev/null && eval "$(mise x -- fzf --zsh)"
fi

bindkey '^I' expand-or-complete

# man zshcompsys
# zstyle ':completion:<function>:<completer>:<command>:<argument>:<tag>' style value
#   - tag: files, domains, users, options
#   - :completion:*:*:cp:* == :completion:*:cp:*
zstyle ':completion:*' completer _extensions _complete _approximate
zstyle ':completion:*' group-name ''
zstyle ':completion:*:default' list-colors ${(s.:.)LS_COLORS}
zstyle ':completion:*:*:*:*:descriptions' format '%F{green}-- %d --%f'
zstyle ':completion:*:*:*:*:corrections' format '%F{yellow}!- %d (errors: %e) -!%f'

_sq_completion() { eval "$(sq completion zsh)" }
_comps[sq]=_sq_completion

_gh_completion() { eval "$(gh completion -s zsh)" }
_comps[gh]=_gh_completion

_op_completion() { eval "$(op completion zsh)" }
_comps[op]=_op_completion

################################
# fzf
################################

if [ "$TERM_PROGRAM" = zed ]; then
  export FZF_DEFAULT_OPTS="--extended --cycle --reverse --ansi"
else
  export FZF_DEFAULT_OPTS="--extended --cycle --reverse --ansi --tmux 90%,70%"
fi

export FZF_COMPLETION_OPTS=""
export FZF_COMPLETION_TRIGGER=""

bindkey '^F' fzf-completion

#### command complete
# elif eval "noglob type _fzf_complete_${cmd_word} >/dev/null"; then
#   prefix="$prefix" eval _fzf_complete_${cmd_word} ${(q)lbuf}
#   zle reset-prompt

_fzf_compgen_path() {
  fd --type f --no-ignore-vcs --hidden --follow . "$1"
}

_fzf_compgen_dir() {
  fd --type d --no-ignore-vcs --hidden --follow . "$1"
}

_fzf_complete_make() {
  _fzf_complete -m -- "$@" < <(
    make help
  )
}
_fzf_complete_make_post() {
  awk '{print $1}'
}

_fzf_complete_m() {
  _fzf_complete --min-height 15 -- "$@" < <(
    mise tasks ls --no-header
  )
}
_fzf_complete_m_post() {
  awk '{print $1}'
}

#### history
_fzf_select_history_widget() {
  BUFFER="$(history | perl -e 'print reverse <>' |
    perl -pe 's/^\s*\d+\*?\s+//' |
    awk '!a[$0]++ && 10<length && length<256' |
    grep -v 'git add' |
    fzf --height=40% --scheme=history --no-sort --query "$LBUFFER" |
    sed 's/\\n/\n/')"
  CURSOR=$#BUFFER             # cursor move to line end
  zle reset-prompt
}
zle -N _fzf_select_history_widget
bindkey '^R' _fzf_select_history_widget

#### memorize
_fzf_default_completion() {
  BUFFER="$(
    cat ~/dots/memorize |
      perl -ne 'print if !/^(#|$)/' |
      perl -pe 's/(## .+)/\e[0;32m\1\e[0m/' |
      fzf --preview='echo {} | perl -pe "s/^(.+)\s*(## .+)/\\2\n\\1/; s/;;/\n##/g;"' \
          --preview-window='down:~10:wrap' \
          --min-height 15 \
          --height ~80% \
          --scheme=history \
          --query "$LBUFFER" |
      perl -pe 's/## .+?$//'
  )"
  CURSOR=$#BUFFER
  zle reset-prompt
}
zle -N _fzf_default_completion
export fzf_default_completion=_fzf_default_completion

#### memorize pipe
_fzf_pipe_complete_post() { perl -pe 's/## .+?$//' }
_fzf_pipe_complete() {
  _fzf_complete -m --scheme=history --preview 'echo {} | perl -pe "s/^(.+)(\s+## .+)/\\2\n\\1/"' --preview-window down:3:wrap --min-height 15 -- "$@" < <(
    cat ~/dots/memorize |
      perl -ne 'print if !/^(#|$)/ && /^\|/' |
      perl -pe 's/(## .+)/\e[0;32m\1\e[0m/'
  )
}

#### memorize sub
_fzf_sub_complete_post() { perl -pe 's/## .+?$//' }
_fzf_sub_complete() {
  _fzf_complete -m --scheme=history --preview 'echo {} | perl -pe "s/^(.+)(\s+## .+)/\\2\n\\1/"' --preview-window down:3:wrap --min-height 15 -- "$@" < <(
    cat ~/dots/memorize |
      perl -ne 'print if !/^(#|$)/ && /^\$\(/' |
      perl -pe 's/(## .+)/\e[0;32m\1\e[0m/'
  )
}

#### command complete
_fzf_command_complete_g_post() { awk '{ print $2 }' }
_fzf_command_complete_g() {
  _fzf_complete -m --preview 'echo {}' --preview-window down:3:wrap --min-height 15 -- "$@" < <(
    git status --short
  )
}

_fzf_command_complete_gb_post() { perl -pe 's/^\*//' | awk '{print $2}' }
_fzf_command_complete_gb() {
  _fzf_complete -m --preview 'echo {}' --tac --preview-window down:3:wrap --min-height 15 -- "$@" < <(
    git for-each-ref --sort=committerdate refs/heads/ --color=always \
      --format="%(HEAD) %(color:green)%(committerdate:short)%(color:reset) %09 %(color:yellow)%(refname:short)%(color:reset) %09 %(authorname) %09 %(contents:subject)" \
      | column -t -s $'\t'
  )
}

_fzf_command_complete_di_post() { awk '{ print $3 }' }
_fzf_command_complete_di() {
  _fzf_complete -m --preview 'echo {} | awk "{print $3}" | xargs docker image inspect' --preview-window right:40%:wrap --min-height 15 -- "$@" < <(
    docker images
  )
}

_fzf_command_complete_dc_post() { awk '{ print $1 }' }
_fzf_command_complete_dc() {
  _fzf_complete -m --preview 'echo {}' --preview-window down:3:wrap --min-height 15 -- "$@" < <(
    docker container ls
  )
}

_fzf_command_complete_rise_dir() {
  _fzf_complete -m --preview 'ls {}' --preview-window down:3:wrap --min-height 15 -- "$@" < <(
    perl -e "
        chomp(\$dir = qx(pwd));
        chomp(\$dir = qx(dirname \$dir)) for (1..2);
        print qq(\$dir\n) and chomp(\$dir = qx(dirname \$dir)) while \$dir ne qq(/)"
  )
}

_fzf_pane_complete() {
  _fzf_complete -m --min-height 15 -- "$@" < <(
  tmux capture-pane -J -p -t $TMUX_PANE | perl -pe 's/\s+/\n/g;' | awk '!a[$0]++ && length($0)>8' | sort
  )
}

_fzf_jq_repl_complete() {
  local query ret
  query=$(jq-repl -- $(perl -pe 's/ *\| *jq$//' <<<"$LBUFFER"))
  ret=$?

  if [ $ret -eq 130 ]; then # Ignore SIGINT
    return 0
  fi

  if [ -n "$query" ]; then
    [[ -z "$JQ_REPL_ARGS" ]] || LBUFFER="${LBUFFER} ${JQ_REPL_ARGS}"
    LBUFFER="${LBUFFER} '$query'"
  fi

  return ret
}

# if return 0 -> hook success, ignore
# if return 1 -> hook fail. run default fzf completion
_fzf_my_completion_hook() {
  local prefix lbuf
  prefix=$1
  lbuf=$2

  case $prefix in
    "|"   ) prefix="" eval _fzf_pipe_complete ${(q)lbuf} ;;
    "?"   ) prefix="" eval _fzf_pane_complete ${(q)lbuf} ;;
    '$('  ) prefix="" eval _fzf_sub_complete ${(q)lbuf} ;;
    '..'  ) prefix="" eval _fzf_command_complete_rise_dir ${(q)lbuf} ;;
    'jq'  ) _fzf_jq_repl_complete; return $? ;;
    # 'grep') _fzf_grep_repl_complete; return $? ;; # TODO
    :*    )
      # TODO: :g -> c-f 동작이 불편, 다른 트리거로 변경 _ls? @g?
      if eval "type _fzf_command_complete_${prefix#*:} > /dev/null"; then
        prefix="" eval _fzf_command_complete_${prefix#*:} ${(q)lbuf}
      else
        return 1
      fi
    ;;

    *) return 1
  esac
}

# _fzf_complete_${cmd_word}
# $1 : prefix, $2 : lbuf
_fzf_path_completion () {
  if _fzf_my_completion_hook "$1" "$2"; then
    zle reset-prompt
  else
    __fzf_generic_path_completion "$1" "$2" _fzf_compgen_path "-m" "" " "
  fi
}

################################

[ "$RUN_ZSH_PROFILE" = "true" ] && zprof > "$HOME/tmp/prof/$(date +"%Y%m%dT%H%M%S").log"
