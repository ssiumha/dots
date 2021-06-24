if ! type fzf &>/dev/null;
then
    echo 'fzf.zsh: not found fzf\n'
    return 1
fi

export FZF_DEFAULT_OPTS="--extended --cycle --reverse --height=40% --ansi"
type rg &> /dev/null && export FZF_DEFAULT_COMMAND='rg --files --hidden'

# --preview $LINES, ECOLUMNS {+} or {-1..n} or {n}
# --preview-window=up:n
# +s : no sort
# -m : multi


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

fzf-select-history() {
  BUFFER="$(history | perl -e 'print reverse <>' |
    perl -pe 's/^\s*\d+\*?\s+//' |
    awk '!a[$0]++' |
    awk 'length<256' |
    fzf --no-sort --query "$LBUFFER" |
    sed 's/\\n/\n/')"
  CURSOR=$#BUFFER             # cursor move to line end
  zle reset-prompt
}
zle -N fzf-select-history     # register widget
bindkey '^R' fzf-select-history


fzf-favorite() {
  BUFFER="$(cat $ZSH/favorite.txt | fzf --query "$LBUFFER" | sed 's/\\n/\n/')"
  CURSOR=$#BUFFER
  zle reset-prompt
}
zle -N fzf-favorite
bindkey '^F' fzf-favorite

# TODO LBUFFER control
# fzf-make() {
#   BUFFER="$(make -f ~/scripts/tower/Makefile help | fzf --query "$LBUFFER")"
#   CURSOR=$#BUFFER
#   zle reset-prompt
# }
# zle -N fzf-make
# bindkey '^[m' fzf-make


fzf-ssh() {
  # ssh $@ "$(cat ~/.ssh/config | grep ^Host | awk '{print $2}' |
  #    fzf --query "$LBUFFER" | sed 's/\\n/\n/')"
  BUFFER="$(cat ~/.ssh/config | grep ^Host | awk '{print $2}' |
      fzf --query "$LBUFFER" | sed 's/\\n/\n/')"
}


fzf-move-path() {
  #if git rev-parse 2> /dev/null; then
  #  source_files=$(git ls-files)
  #else
  #  source_files=$(find . -type f)
  #fi
  cd "$(find . -type d | egrep -v '(.git|.svn)' |
      awk 'NR>1' | fzf --query "$LBUFFER")"
}
alias -g cdf='fzf-move-path'

mru() {
  cd "$(cat ~/.zsh_cdhistory 2>/dev/null |
    egrep -v '(.git|.svn)' | sort -u | fzf --query "$LBUFFER")"
}

# fd() {
#    local dir;
#    dir=$(find ${1:-.} -path '*/\.*' -prune -o -type d -print 2> /dev/null | fzf +m) &&
#    cd "$dir";
#}


alias -g gB='$(git branch -a | fzf --prompt "GIT BRANCH>" | sed -e "s/^\*\s*//g")'
alias -g gR='$(git remote -a | fzf --prompt "GIT REMOTE>")'
alias -g gH='$(git log --oneline --branches | fzf --prompt "GIT HASH>" | awk "{print \$1}")'
alias -g gS='$( \
    git status --short \
    | fzf +s \
        --preview="echo {} | sed -e \"s/^.. //\" | xargs git --no-pager diff" \
        --multi --ansi --prompt "GIT STATUS>" \
    | sed -e "s/^.. //" \
)'

# preview-page-up, preview-page-down : <S-up>, <S-down>

# Custom Completion {{{
alias to="make -f $HOME/scripts/tower/makefile"
_fzf_complete_to() {
  _fzf_complete -m --preview 'echo {}' --preview-window down:3:wrap --min-height 15 -- "$@" < <(
    to help
  )
}

_fzf_complete_to_post() {
  awk '{print $1}'
}


_fzf_complete_make() {
  _fzf_complete -m --preview 'echo {}' --preview-window down:3:wrap --min-height 15 -- "$@" < <(
    command make help
  )
}

_fzf_complete_make_post() {
  awk '{print $1}'
}

# ref: fzf-completion
export fzf_default_completion=_fzf_default_completion
_fzf_default_completion() {
  if [ ${#tokens} -lt 1 ]; then
    zle expand-or-complete
    return
  fi

  if [ "$cmd" = make -o "$cmd" = to ] && [ ${LBUFFER[-1]} = ' ' ]; then
    tail=$trigger
    tokens+=$trigger
    lbuf="$lbuf$trigger"

    d_cmds=(${=FZF_COMPLETION_DIR_COMMANDS:-cd pushd rmdir})

    [ -z "$trigger"      ] && prefix=${tokens[-1]} || prefix=${tokens[-1]:0:-${#trigger}}
    [ -n "${tokens[-1]}" ] && lbuf=${lbuf:0:-${#tokens[-1]}}

    prefix="$prefix" eval _fzf_complete_${cmd} ${(q)lbuf}
    return
  fi

  zle expand-or-complete
}
zle -N _fzf_default_completion

# }}}

# check shell is interactive mode
[[ $- == *i* ]] \
    && source "$HOME/.local/repo/fzf/shell/completion.zsh" 2> /dev/null

#source "$HOME/.local/repo/fzf/shell/key-bindings.zsh"

return 0;
