if ! type fzf &>/dev/null;
then
    echo 'fzf.zsh: not found fzf\n'
    return 1
fi


export FZF_DEFAULT_OPTS="--extended --cycle --reverse --height=40% --ansi"

[[ $- == *i* ]] && source "$HOME/.local/repo/fzf/shell/completion.zsh" 2> /dev/null
#source "$HOME/.local/repo/fzf/shell/key-bindings.zsh"



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
    fzf --query "$LBUFFER" |
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
alias -g c='fzf-move-path'

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
#alias -g gS='$(git status --short | fzf-tmux +s --multi --ansi --prompt "GIT STATUS>" | sed -e "s/^.. //")'

return 0;
