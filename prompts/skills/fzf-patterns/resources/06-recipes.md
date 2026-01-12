# fzf 실용 레시피

실전에서 바로 사용 가능한 fzf 함수 모음입니다.

## 파일 작업

### fe - Fuzzy Edit

```bash
fe() {
  local file=$(
    fd --type f --hidden --follow --exclude .git |
    fzf --preview 'bat --color=always --style=numbers {}'
  )
  [[ -n "$file" ]] && ${EDITOR:-vim} "$file"
}
```

### fcd - Fuzzy cd

```bash
fcd() {
  local dir=$(
    fd --type d --hidden --follow --exclude .git |
    fzf --preview 'tree -C {} | head -100'
  )
  [[ -n "$dir" ]] && cd "$dir"
}
```

### fo - Fuzzy Open (macOS)

```bash
fo() {
  local file=$(
    fd --type f |
    fzf --preview 'bat --color=always {}'
  )
  [[ -n "$file" ]] && open "$file"
}
```

## Git 작업

### fbr - Fuzzy Branch

```bash
fbr() {
  local branch=$(
    git branch -a --color=always |
    fzf --ansi --preview 'git log --oneline --graph --color=always {-1}' |
    sed 's/^[* ]*//' |
    sed 's#remotes/origin/##'
  )
  [[ -n "$branch" ]] && git checkout "$branch"
}
```

### fco - Fuzzy Checkout (branches + tags)

```bash
fco() {
  local target=$(
    { git branch --color=always; git tag --color=always; } |
    fzf --ansi --preview 'git log --oneline -20 --color=always {-1}'
  )
  target=$(echo "$target" | sed 's/^[* ]*//')
  [[ -n "$target" ]] && git checkout "$target"
}
```

### flog - Fuzzy Git Log

```bash
flog() {
  git log --oneline --graph --color=always |
  fzf --ansi --no-sort --reverse \
      --preview 'git show --color=always {1}' \
      --bind 'enter:become:git show {1}'
}
```

### fstash - Fuzzy Stash

```bash
fstash() {
  local stash=$(
    git stash list |
    fzf --preview 'git stash show -p --color=always {1}'
  )
  [[ -n "$stash" ]] && git stash apply $(echo "$stash" | cut -d: -f1)
}
```

### fadd - Fuzzy Git Add

```bash
fadd() {
  local files=$(
    git status --short |
    fzf --multi --preview 'git diff --color=always {2}' |
    awk '{print $2}'
  )
  [[ -n "$files" ]] && git add $(echo "$files" | tr '\n' ' ')
}
```

## 프로세스 관리

### fkill - Fuzzy Kill

```bash
fkill() {
  local pid=$(
    ps -ef |
    sed 1d |
    fzf --multi --preview 'ps -p {2} -o pid,ppid,%cpu,%mem,command' |
    awk '{print $2}'
  )
  [[ -n "$pid" ]] && echo "$pid" | xargs kill -${1:-9}
}
```

### fport - Fuzzy Port Kill

```bash
fport() {
  local pid=$(
    lsof -i -P -n |
    fzf --header-lines=1 --multi |
    awk '{print $2}'
  )
  [[ -n "$pid" ]] && echo "$pid" | xargs kill -9
}
```

## 검색

### rfv - Ripgrep + fzf + Vim

```bash
rfv() {
  local RELOAD='reload:rg --column --color=always --smart-case {q} || :'
  local OPENER='if [[ $FZF_SELECT_COUNT -eq 0 ]]; then
                  vim {1} +{2}
                else
                  vim +cw -q {+f}
                fi'

  fzf --disabled --ansi --multi \
      --bind "start:$RELOAD" \
      --bind "change:$RELOAD" \
      --bind "enter:become:$OPENER" \
      --bind "ctrl-o:execute:$OPENER" \
      --bind 'alt-a:select-all,alt-d:deselect-all,ctrl-/:toggle-preview' \
      --delimiter : \
      --preview 'bat --style=full --color=always --highlight-line {2} {1}' \
      --preview-window '~4,+{2}+3/3,<80(up)' \
      --query "$*"
}
```

### rg-fzf - 모드 전환

```bash
rg-fzf() {
  local RG_PREFIX="rg --column --line-number --no-heading --color=always --smart-case"
  local INITIAL_QUERY="${*:-}"

  fzf --ansi --disabled --query "$INITIAL_QUERY" \
      --bind "start:reload:$RG_PREFIX {q}" \
      --bind "change:reload:sleep 0.1; $RG_PREFIX {q} || true" \
      --bind 'ctrl-t:transform:[[ ! $FZF_PROMPT =~ rg ]] &&
        echo "rebind(change)+change-prompt(1. rg> )+disable-search+transform-query:echo \{q} > /tmp/rg-fzf-f; cat /tmp/rg-fzf-r" ||
        echo "unbind(change)+change-prompt(2. fzf> )+enable-search+transform-query:echo \{q} > /tmp/rg-fzf-r; cat /tmp/rg-fzf-f"' \
      --prompt '1. rg> ' \
      --delimiter : \
      --preview 'bat --color=always {1} --highlight-line {2}' \
      --preview-window 'up,60%,border-bottom,+{2}+3/3,~3' \
      --bind 'enter:become(vim {1} +{2})'
}
```

## tmux

### tm - Tmux Session

```bash
tm() {
  [[ -n "$TMUX" ]] && change="switch-client" || change="attach-session"

  if [[ $1 ]]; then
    tmux $change -t "$1" 2>/dev/null || (tmux new-session -d -s "$1" && tmux $change -t "$1")
    return
  fi

  local session=$(
    tmux list-sessions -F "#{session_name}" 2>/dev/null |
    fzf --exit-0
  ) && tmux $change -t "$session" || echo "No sessions"
}
```

### ftpane - Tmux Pane

```bash
ftpane() {
  local pane=$(
    tmux list-panes -a -F "#{session_name}:#{window_index}.#{pane_index} #{pane_current_path} #{pane_current_command}" |
    fzf --preview 'tmux capture-pane -ep -t {1}'
  )
  [[ -n "$pane" ]] && tmux switch-client -t $(echo "$pane" | awk '{print $1}')
}
```

## Docker

### dex - Docker Exec

```bash
dex() {
  local container=$(
    docker ps --format "table {{.ID}}\t{{.Names}}\t{{.Image}}\t{{.Status}}" |
    fzf --header-lines=1 |
    awk '{print $1}'
  )
  [[ -n "$container" ]] && docker exec -it "$container" ${1:-/bin/sh}
}
```

### dlog - Docker Logs

```bash
dlog() {
  local container=$(
    docker ps -a --format "table {{.ID}}\t{{.Names}}\t{{.Image}}\t{{.Status}}" |
    fzf --header-lines=1 |
    awk '{print $1}'
  )
  [[ -n "$container" ]] && docker logs -f "$container"
}
```

### drm - Docker Remove

```bash
drm() {
  local containers=$(
    docker ps -a --format "table {{.ID}}\t{{.Names}}\t{{.Image}}\t{{.Status}}" |
    fzf --header-lines=1 --multi |
    awk '{print $1}'
  )
  [[ -n "$containers" ]] && echo "$containers" | xargs docker rm
}
```

## 로그 스트림

### flog-stream - 로그 실시간 검색

```bash
flog-stream() {
  tail -f "$1" |
  fzf --tail 100000 --tac --no-sort --exact --wrap \
      --bind 'ctrl-c:abort'
}
```

### kubectl-logs - K8s 로그

```bash
kubectl-logs() {
  local pod=$(
    kubectl get pods |
    fzf --header-lines=1 --preview 'kubectl describe pod {1}' |
    awk '{print $1}'
  )
  [[ -n "$pod" ]] && kubectl logs -f "$pod"
}
```

## 함수 템플릿

새 fzf 함수 작성 시 참고:

```bash
my_fzf_func() {
  local selected=$(
    SOURCE_COMMAND |
    fzf --height 40% \
        --layout reverse \
        --preview 'PREVIEW_COMMAND {}' \
        --bind 'enter:become:ACTION {}'
  )
  [[ -n "$selected" ]] && FINAL_ACTION "$selected"
}
```

## 플랫폼 호환성

### 클립보드

```bash
# macOS
pbcopy / pbpaste

# Linux (xclip)
xclip -selection clipboard

# 크로스 플랫폼 함수
clip() {
  if command -v pbcopy &>/dev/null; then
    pbcopy
  elif command -v xclip &>/dev/null; then
    xclip -selection clipboard
  fi
}

# 사용
--bind 'ctrl-y:execute-silent(echo {} | clip)'
```
