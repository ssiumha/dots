# vim: et ts=2

# time RUN_ZSH_PROFILE=true zsh -i -c exit
[ "$RUN_ZSH_PROFILE" = "true" ] && zmodload zsh/zprof

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
alias ls="ls $LS_OPTIONS"
####


################################
# Alias
################################
alias mv="mv -i"
alias cp="cp -i"

alias g="git"

# alias -g ...="../.." # TODO fzf

if command -v nvim &>/dev/null; then
  alias v="nvim"
  export EDITOR="nvim"
else
  alias v="vim"
  export EDITOR="vim"
fi

if command -v exa &>/dev/null; then
  alias l="exa -s type"
  alias la="exa -s type -a"
  alias ll="exa -s type -l"
  alias llg="exa -s type -l --git"
  alias lla="exa -s type -la"
  alias lt="exa -s type --tree -l"
else
  alias l="ls -lh"
  alias la="ls -Ah"
  alias ll="ls -lh"
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

  reset_color="\e[49m\e[39m"

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

[ -d "$HOME/.asdf" ] && source "$HOME/.asdf/asdf.sh"

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
  autoload -Uz _zinit
  (( ${+_comps} )) && _comps[zinit]=_zinit

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

autoload -Uz compinit; compinit

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

# ignore chitoku-k/fzf-zsh-completions
_fzf_complete_colorize() { cat; return; }

#### command complete

# _fzf_compgen_path() {
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
zle -N fzf_select_history_widget
bindkey '^R' fzf_select_history_widget

#### favcmd
_fzf_default_completion() {
  BUFFER="$(
    cat ~/spells/favcmd |
      perl -ne 'print if !/^(#|$)/' |
      perl -pe 's/(## .+)/\e[0;32m\1\e[0m/' |
      fzf --preview='echo {} | perl -pe "s/^(.+)(## .+)/\\2\n\\1/"' \
          --preview-window=wrap \
          --scheme=history \
          --query "$LBUFFER" |
      perl -pe 's/## .+?$//'
  )"
  CURSOR=$#BUFFER
  zle reset-prompt
}
zle -N _fzf_default_completion
export fzf_default_completion=_fzf_default_completion

################################

[ "$RUN_ZSH_PROFILE" = "true" ] && zprof
