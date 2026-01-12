# fzf 쉘 통합

fzf의 쉘 통합 기능으로 일상적인 작업을 크게 개선할 수 있습니다.

## 설정

### Bash

```bash
# ~/.bashrc
eval "$(fzf --bash)"
```

### Zsh

```bash
# ~/.zshrc
source <(fzf --zsh)
```

### Fish

```fish
# ~/.config/fish/config.fish
fzf --fish | source
```

**중요**: 환경변수는 이 설정 **이전에** 정의해야 적용됩니다.

## 기본 키 바인딩

| 키 | 기능 |
|----|------|
| `CTRL-R` | 명령어 히스토리 검색 |
| `CTRL-T` | 파일/디렉토리 선택하여 삽입 |
| `ALT-C` | 디렉토리 선택하여 cd |

## 환경 변수

### 기본 옵션

```bash
# 모든 fzf 호출에 적용
export FZF_DEFAULT_OPTS='
  --height 40%
  --layout reverse
  --border
  --info inline
'

# 파일에서 옵션 로드
export FZF_DEFAULT_OPTS_FILE=~/.fzfrc
```

### 기본 명령어

```bash
# 기본 입력 소스
export FZF_DEFAULT_COMMAND='fd --type f --hidden --follow --exclude .git'
```

### CTRL-T 커스텀

```bash
# 명령어
export FZF_CTRL_T_COMMAND='fd --type f --hidden --follow --exclude .git'

# 옵션
export FZF_CTRL_T_OPTS='
  --preview "bat --color=always --style=numbers --line-range=:500 {}"
  --bind "ctrl-/:toggle-preview"
  --walker-skip .git,node_modules,target
'
```

### CTRL-R 커스텀

```bash
export FZF_CTRL_R_OPTS='
  --preview "echo {}"
  --preview-window up:3:hidden:wrap
  --bind "ctrl-/:toggle-preview"
  --bind "ctrl-y:execute-silent(echo -n {2..} | pbcopy)+abort"
  --color header:italic
  --header "Press CTRL-Y to copy command into clipboard"
'
```

### ALT-C 커스텀

```bash
# 명령어
export FZF_ALT_C_COMMAND='fd --type d --hidden --follow --exclude .git'

# 옵션
export FZF_ALT_C_OPTS='
  --preview "tree -C {} | head -100"
'
```

## Fuzzy Completion

`**`로 트리거되는 fuzzy 완성:

```bash
# 파일/디렉토리
vim **<TAB>
cd **<TAB>

# 프로세스 ID
kill **<TAB>

# 호스트명
ssh **<TAB>
telnet **<TAB>

# 환경 변수
export **<TAB>
unset **<TAB>
```

### Completion 커스텀

```bash
# 트리거 변경 (기본: **)
export FZF_COMPLETION_TRIGGER='~~'

# 옵션
export FZF_COMPLETION_OPTS='--border --info=inline'

# 경로 완성 옵션
export FZF_COMPLETION_PATH_OPTS='--walker file,dir,hidden,follow'

# 디렉토리 완성 옵션
export FZF_COMPLETION_DIR_OPTS='--walker dir,hidden,follow'
```

## 커스텀 함수

### z/zoxide 통합

```bash
# z 통합
z() {
  local dir=$(
    _z 2>&1 |
    fzf --height 40% --layout reverse --info inline \
        --nth 2.. --tac --no-sort --query "$*" \
        --accept-nth 2..
  ) && cd "$dir"
}

# zoxide 통합
z() {
  local dir=$(
    zoxide query --list --score |
    fzf --height 40% --layout reverse --info inline \
        --nth 2.. --tac --no-sort --query "$*" \
        --bind 'enter:become:echo {2..}'
  ) && cd "$dir"
}
```

### 히스토리에서 실행

```bash
fh() {
  eval $(history | fzf +s --tac | sed 's/ *[0-9]* *//')
}
```

## 비활성화

특정 바인딩 비활성화:

```bash
# CTRL-T 비활성화
export FZF_CTRL_T_COMMAND=''

# ALT-C 비활성화
export FZF_ALT_C_COMMAND=''
```

## 추천 설정

```bash
# ~/.zshrc 또는 ~/.bashrc

# 기본 옵션
export FZF_DEFAULT_OPTS='
  --height 50%
  --layout reverse
  --border rounded
  --info inline
  --preview-window right:50%:wrap
  --bind ctrl-/:toggle-preview
  --bind ctrl-a:select-all
  --bind ctrl-d:deselect-all
'

# fd 사용 (더 빠름)
export FZF_DEFAULT_COMMAND='fd --type f --hidden --follow --exclude .git'
export FZF_CTRL_T_COMMAND="$FZF_DEFAULT_COMMAND"
export FZF_ALT_C_COMMAND='fd --type d --hidden --follow --exclude .git'

# 프리뷰 설정
export FZF_CTRL_T_OPTS='--preview "bat --color=always --style=numbers {}"'
export FZF_ALT_C_OPTS='--preview "tree -C {} | head -100"'

# 쉘 통합 로드
source <(fzf --zsh)  # 또는 eval "$(fzf --bash)"
```
