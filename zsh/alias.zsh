#alias history='fc -il 1'
alias history='fc -l 1'

alias mv="mv -i"
alias cp="cp -i"


alias ll="ls -lAh"
alias l="ls -lh"

l_func() {
  LS_OPTIONS=--color=always CLICOLOR_FORCE=1 ls -lh $@ | awk '
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

if type nvim &>/dev/null; then
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

alias rg="rg --no-ignore"

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
