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

cmd_current_time() {
  echo "$(date +%H:%M:%S)"
}

git_repo_info() {
  if ! git rev-parse --quiet --verify HEAD &>/dev/null; then
    return
  fi

  local branch=$(git rev-parse --abbrev-ref HEAD)
  local changes=$(git diff-index --quiet HEAD || echo '!')
  local added=$(git diff-index --cached --diff-filter=A --quiet HEAD || echo '+')

  # ex) master+!
  printf " $branch$added$changes "
}

kube_info() {
  test kubectl && kubectl config get-contexts | perl -nale 'print @F[1] =~ s!.+/(.+)!$1!r if /^\*/'
}

chpwd() {
  [ ! -f "$HOME/.local/zsh/cdhistory" ] && touch "$HOME/.local/zsh/cdhistory"
  [ "`pwd`" != ~ ] && perl -i'' -ne 'print `pwd` if $. == 1; print if 1..9999' "$HOME/.local/zsh/cdhistory"
}

preexec() {
  cmd_timestamp=`date +%s`
}

precmd() {
  #[%D{%y-%d-%m %H:%M}]
  # TODO : %~ coloring. symbolic:cyan(6), current:bold?
  # %F-fg, %K-bg, %S-reverse
  reset_color="\e[49m\e[39m"
  txt="\n"
  if [[ -n $SSH_CLIENT ]]; then
    txt+="%K{10} ${reset_color}"
    # RPROMPT="%F{8}${SSH_TTY:+%n@%m}%f"
  fi
  if [ ! -n "$TMUX" ]; then
    txt+="%K{8} %n@%m ${reset_color}"
  fi
  txt+="%K{0} %~ ${reset_color}"
  txt+="%K{8}$(git_repo_info)${reset_color}"
  txt+=" %F{11}$(cmd_current_time)"

  # [[ ! -z $PYENV_VERSION ]] && txt+="%F{7}py:${PYENV_VERSION}%F{0}"
  if [[ ! -z $VIRTUAL_ENV ]]; then
    txt+="        %F{7}venv:${VIRTUAL_ENV##*/}%F{0}"
  fi

  print -P $txt
  cmd_timestamp=0
}

# tmux를 통한 커맨드 복붙 동작이 불편해져서 봉인..
# rprompt_func() { }
# RPROMPT='$(rprompt_func)'

# TODO working only login zsh?
# TODO respond bash?
export VIRTUAL_ENV_DISABLE_PROMPT=1
PROMPT='%(?.%F{13}.%F{1})❯%f '
PS1=$PROMPT
