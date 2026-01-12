# fzf 프리뷰 설정

프리뷰 창으로 선택 전 컨텍스트를 확인할 수 있습니다.

## 기본 사용법

```bash
fzf --preview 'COMMAND {}'
```

`{}`는 현재 선택된 항목으로 치환됩니다.

## 프리뷰 창 옵션

```bash
--preview-window [POSITION][,SIZE][,OPTIONS]
```

### 위치

| 값 | 설명 |
|----|------|
| `right` | 오른쪽 (기본) |
| `left` | 왼쪽 |
| `up` | 위 |
| `down` | 아래 |

### 크기

```bash
--preview-window right:50%     # 50%
--preview-window right:60      # 60 컬럼
--preview-window 'right,60%'   # 쉼표 구분도 가능
```

### 옵션

| 옵션 | 설명 |
|------|------|
| `wrap` | 줄바꿈 |
| `nowrap` | 줄바꿈 안함 |
| `hidden` | 숨김 상태로 시작 |
| `follow` | 스트림 끝 추적 (tail -f처럼) |
| `cycle` | 스크롤 순환 |
| `border` | 프리뷰 테두리 |
| `noborder` | 테두리 없음 |
| `border-STYLE` | 특정 테두리 스타일 |

### 조합 예시

```bash
# 오른쪽 50%, 줄바꿈, 테두리
--preview-window 'right:50%:wrap:border-left'

# 아래 40%, 숨김 시작
--preview-window 'down:40%:hidden'

# 반응형: 80컬럼 미만이면 위로
--preview-window 'right:50%,<80(up:40%)'
```

## 스크롤 오프셋

특정 위치로 스크롤하여 표시:

```bash
# {2}번 라인 중심으로
--preview-window '+{2}-5'

# 분수로 위치 지정 (1/3 지점)
--preview-window '+{2}+3/3'
```

### 헤더 고정

```bash
# 상단 3줄 고정
--preview-window '~3'
```

## 자주 쓰는 프리뷰 명령

### 파일 내용

```bash
# bat (구문 강조)
--preview 'bat --color=always --style=numbers --line-range=:500 {}'

# cat 대체
--preview 'head -500 {}'

# 특정 라인 강조 (ripgrep 결과용)
--preview 'bat --color=always --highlight-line {2} {1}'
```

### 디렉토리

```bash
# tree
--preview 'tree -C {} | head -100'

# eza/exa
--preview 'eza --tree --level=2 --color=always {}'
```

### Git

```bash
# 브랜치 로그
--preview 'git log --oneline --graph --color=always {}'

# 커밋 상세
--preview 'git show --color=always {}'

# diff
--preview 'git diff --color=always {}'

# 파일 변경 내역
--preview 'git log --oneline --follow --color=always -- {}'
```

### 프로세스

```bash
# 프로세스 상세 (macOS)
--preview 'ps -p {2} -o pid,ppid,%cpu,%mem,command'
```

### 이미지

```bash
# Kitty 터미널
--preview 'kitty +kitten icat {}'

# iTerm2
--preview 'imgcat {}'

# chafa (범용)
--preview 'chafa -s 80x24 {}'
```

## 고급 패턴

### 파일 타입별 분기

```bash
--preview '
  if [[ -d {} ]]; then
    tree -C {} | head -100
  elif [[ -f {} ]]; then
    bat --color=always {}
  fi
'
```

### ripgrep 통합 (라인 번호 포함)

```bash
# 형식: file:line:column:content
--delimiter :
--preview 'bat --color=always --highlight-line {2} {1}'
--preview-window '+{2}+3/3,~3'
```

### 동적 프리뷰 변경

```bash
--bind 'ctrl-/:change-preview-window(down|hidden|right)'
--bind 'ctrl-p:change-preview(cat {})'
--bind 'ctrl-b:change-preview(bat --color=always {})'
```

## 프리뷰 성능 팁

1. **파일 크기 제한**: `head -N` 또는 `--line-range`
2. **캐시 활용**: bat의 `--paging=never`
3. **비동기 로딩**: fzf가 자동 처리
4. **조건부 표시**: 파일 존재 확인 후 실행

```bash
--preview '[[ -f {} ]] && bat --color=always {} || echo "Not a file"'
```
