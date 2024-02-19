# vim: et ts=2

# time RUN_ZSH_PROFILE=true zsh -i -c exit
[ "$RUN_ZSH_PROFILE" = "true" ] && zmodload zsh/zprof

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

################################
# Path
################################
echo $PATH | grep -q "$HOME/.local/bin" && : || export PATH="$HOME/.local/bin:$PATH"

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
####


################################
# Alias
################################
alias mv="mv -i"
alias cp="cp -i"

alias g="git"

alias -g ...=../..
alias -g ....=../../..

if command -v nvim &>/dev/null; then
  alias v="nvim"
  export EDITOR="nvim"
else
  alias v="vim"
  export EDITOR="vim"
fi

if command -v lsd &>/dev/null; then
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

alias k="kubectl"
ksw() {
  kubectl config get-contexts \
    | perl -nale 'next if $.==1; s/^(.)\s+([^\s]+).+$/$1 $2/; print' \
    | fzf | cut -c3- | xargs -I% kubectl config use-context %
}

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
elif [ -d "$HOME/.asdf" ]; then
  source "$HOME/.asdf/asdf.sh"
fi

if command -v zoxide &>/dev/null; then
  export _ZO_DATA_DIR="$HOME/.local/zsh/zoxide"
  eval "$(zoxide init zsh --no-cmd)"
  alias z=__zoxide_z
  alias zz=__zoxide_zi
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
  zinit light zsh-users/zsh-syntax-highlighting

  # zinit snippet 'https://github.com/asdf-vm/asdf/blob/master/completions/_asdf'
  zinit snippet 'https://github.com/junegunn/fzf/blob/master/shell/completion.zsh'
  zinit light chitoku-k/fzf-zsh-completions

  zinit snippet 'https://github.com/git/git/blob/master/contrib/completion/git-prompt.sh'
else
  command -v git &>/dev/null \
    && git clone --depth=5 https://github.com/zdharma-continuum/zinit "${zinit_home}/bin"
fi

################################
# completion
################################

if [[ "$_ZSH_INIT_MINIMAL" != true ]]; then
  autoload -Uz compinit; compinit

  command -v mise &>/dev/null && eval "$(mise completion zsh)"
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

################################
# fzf
################################

export FZF_DEFAULT_OPTS="--extended --cycle --reverse --height=40% --ansi"
export FZF_COMPLETION_OPTS=""
export FZF_COMPLETION_TRIGGER=""

bindkey '^F' fzf-completion

#### command complete

# _fzf_dir_completion
#   fd .
# }

_fzf_complete_make() {
  _fzf_complete -m --preview 'echo {}' --preview-window down:3:wrap --min-height 15 -- "$@" < <(
    command make help
  )
}
_fzf_complete_make_post() {
  awk '{print $1}'
}

#### history
_fzf_select_history_widget() {
  BUFFER="$(history | perl -e 'print reverse <>' |
    perl -pe 's/^\s*\d+\*?\s+//' |
    awk '!a[$0]++ && length<256' |
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
    cat ~/dotfiles/memorize |
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
    cat ~/dotfiles/memorize |
      perl -ne 'print if !/^(#|$)/ && /^\|/' |
      perl -pe 's/(## .+)/\e[0;32m\1\e[0m/'
  )
}

#### memorize sub
_fzf_sub_complete_post() { perl -pe 's/## .+?$//' }
_fzf_sub_complete() {
  _fzf_complete -m --scheme=history --preview 'echo {} | perl -pe "s/^(.+)(\s+## .+)/\\2\n\\1/"' --preview-window down:3:wrap --min-height 15 -- "$@" < <(
    cat ~/dotfiles/memorize |
      perl -ne 'print if !/^(#|$)/ && /^\$\(/' |
      perl -pe 's/(## .+)/\e[0;32m\1\e[0m/'
  )
}

#### command complete
_fzf_command_complete_g_post() { awk '{ print $2 }' }
_fzf_command_complete_g() {
  _fzf_complete -m --preview 'echo {}' --preview-window down:3:wrap --min-height 15 -- "$@" < <(
    command git status --short
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
    command docker images
  )
}

_fzf_command_complete_dc_post() { awk '{ print $1 }' }
_fzf_command_complete_dc() {
  _fzf_complete -m --preview 'echo {}' --preview-window down:3:wrap --min-height 15 -- "$@" < <(
    command docker container ls
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

_fzf_my_completion_hook() {
  local prefix lbuf
  prefix=$1
  lbuf=$2

  if [[ "$prefix" == "|" ]]; then
    prefix="" eval _fzf_pipe_complete ${(q)lbuf}
  elif [[ "$prefix" == "\$(" ]]; then
    prefix="" eval _fzf_sub_complete ${(q)lbuf}
  elif [[ "$prefix" == .. ]]; then
    prefix="" eval _fzf_command_complete_rise_dir ${(q)lbuf}
  elif [[ "$prefix" == :* ]]; then
    # TODO: typing -> :g -> c-f 동작이 불편, : 말고 다른 단어가 필요. gst+c-f?
    #    - :l + c-f => current ls
    if eval "type _fzf_command_complete_${prefix#*:} > /dev/null"; then
      prefix="" eval _fzf_command_complete_${prefix#*:} ${(q)lbuf}
    else
      return 1
    fi
  else
    return 1 # default fail
  fi
}

#### overwrite completion
# https://github.com/junegunn/fzf/blob/master/shell/completion.zsh#L264
fzf-completion() {
  local tokens cmd prefix trigger tail matches lbuf d_cmds
  setopt localoptions noshwordsplit noksh_arrays noposixbuiltins

  # http://zsh.sourceforge.net/FAQ/zshfaq03.html
  # http://zsh.sourceforge.net/Doc/Release/Expansion.html#Parameter-Expansion-Flags
  tokens=(${(z)LBUFFER})
  if [ ${#tokens} -lt 1 ]; then
    zle ${fzf_default_completion:-expand-or-complete}
    return
  fi

  cmd=$(__fzf_extract_command "$LBUFFER")

  # Explicitly allow for empty trigger.
  trigger=${FZF_COMPLETION_TRIGGER-'**'}
  [ -z "$trigger" -a ${LBUFFER[-1]} = ' ' ] && tokens+=("")

  # When the trigger starts with ';', it becomes a separate token
  if [[ ${LBUFFER} = *"${tokens[-2]-}${tokens[-1]}" ]]; then
    tokens[-2]="${tokens[-2]-}${tokens[-1]}"
    tokens=(${tokens[0,-2]})
  fi

  lbuf=$LBUFFER
  tail=${LBUFFER:$(( ${#LBUFFER} - ${#trigger} ))}

  # Trigger sequence given
  # if [ ${#tokens} -gt 1 -a "$tail" = "$trigger" ]; then
  if [[ ${#tokens} -gt 1 && ( "$tail" == "$trigger" || "$tail" == :* ) ]]; then
    d_cmds=(${=FZF_COMPLETION_DIR_COMMANDS:-cd pushd rmdir})

    [ -z "$trigger"      ] && prefix=${tokens[-1]} || prefix=${tokens[-1]:0:-${#trigger}}
    [ -n "${tokens[-1]}" ] && lbuf=${lbuf:0:-${#tokens[-1]}}

    if _fzf_my_completion_hook "$prefix" "$lbuf"; then
      zle reset-prompt
    elif eval "type _fzf_complete_${cmd} > /dev/null"; then
      prefix="$prefix" eval _fzf_complete_${cmd} ${(q)lbuf}
      zle reset-prompt
    elif [ ${d_cmds[(i)$cmd]} -le ${#d_cmds} ]; then
      _fzf_dir_completion "$prefix" "$lbuf"
    else
      _fzf_path_completion "$prefix" "$lbuf"
    fi
  # Fall back to default completion
  else
    zle ${fzf_default_completion:-expand-or-complete}
  fi
}

################################

[ "$RUN_ZSH_PROFILE" = "true" ] && zprof
