alias mv="mv -i"
alias cp="cp -i"

alias g="git"

alias rake="noglob rake"
alias rg="rg --no-ignore"

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

if command -v nvim &>/dev/null; then
  alias v="nvim"
else
  alias v="vim"
fi

vzv_func() {
    local T="/tmp/v.amp1.$RANDOM";
    vim \
        +'setl bt=nofile' \
        +'au QuitPre * redi! >'$T'|sil! exe "%pr"|redi END' \
        - >/dev/tty \
    || exit $?
    ;
    cat $T;
    rm -f $T;
}
alias vzv=vzv_func

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

# 자신의 github repo 목록중에서 선택하기
#alias -g H='`curl -sL https://api.github.com/users/YOUR_USERNAME/repos | jq -r ".[].full_name" | peco --prompt "GITHUB REPOS>" | head -n 1`'

#alias ssh='ssh -F <(find ~/.ssh/ -maxdepth 1 -name "*.config" -or -name config -exec cat {} \+)'

