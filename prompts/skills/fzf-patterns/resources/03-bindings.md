# fzf 키 바인딩 (--bind)

`--bind` 옵션으로 키 또는 이벤트에 액션을 바인딩합니다.

## 기본 문법

```bash
--bind 'KEY:ACTION'
--bind 'KEY:ACTION1+ACTION2'  # 체이닝
--bind 'KEY:ACTION(ARG)'      # 인자 전달
```

## 이벤트

| 이벤트 | 발생 시점 |
|--------|----------|
| `start` | fzf 시작 시 (1회) |
| `load` | 입력 로드 완료 시 |
| `change` | 쿼리 변경 시 |
| `search` | 검색 완료 시 |
| `focus` | 포커스 항목 변경 시 |
| `result` | 결과 변경 시 |
| `transform` | 조건부 액션 실행 |
| `resize` | 창 크기 변경 시 |
| `zero` | 결과 0개 시 |
| `one` | 결과 1개 시 |

## 액션 카테고리

### 검색/필터

| 액션 | 설명 |
|------|------|
| `reload:CMD` | 명령 재실행하여 목록 갱신 |
| `reload-sync:CMD` | 동기 reload |
| `enable-search` | 검색 활성화 |
| `disable-search` | 검색 비활성화 |
| `clear-query` | 쿼리 초기화 |
| `change-query:STR` | 쿼리 변경 |

### 선택

| 액션 | 설명 |
|------|------|
| `accept` | 선택 확정 (Enter) |
| `accept-non-empty` | 선택 있을 때만 확정 |
| `select` | 현재 항목 선택 |
| `deselect` | 현재 항목 선택 해제 |
| `toggle` | 선택 토글 |
| `select-all` | 전체 선택 |
| `deselect-all` | 전체 선택 해제 |
| `toggle-all` | 전체 토글 |

### 이동

| 액션 | 설명 |
|------|------|
| `up` / `down` | 위/아래 이동 |
| `first` / `last` | 처음/끝으로 |
| `page-up` / `page-down` | 페이지 이동 |
| `half-page-up/down` | 반 페이지 이동 |
| `jump` | 점프 모드 |
| `jump-accept` | 점프 후 즉시 선택 |

### 프리뷰

| 액션 | 설명 |
|------|------|
| `toggle-preview` | 프리뷰 토글 |
| `preview:CMD` | 프리뷰 명령 변경 |
| `refresh-preview` | 프리뷰 새로고침 |
| `preview-up/down` | 프리뷰 스크롤 |
| `preview-page-up/down` | 프리뷰 페이지 스크롤 |
| `preview-top/bottom` | 프리뷰 처음/끝 |
| `change-preview:CMD` | 프리뷰 명령 교체 |
| `change-preview-window:OPTS` | 프리뷰 창 옵션 변경 |

### 실행

| 액션 | 설명 |
|------|------|
| `execute:CMD` | 명령 실행 (fzf 유지) |
| `execute-silent:CMD` | 조용히 실행 |
| `become:CMD` | fzf를 명령으로 교체 |
| `abort` | 취소 종료 |

### UI 변경

| 액션 | 설명 |
|------|------|
| `change-prompt:STR` | 프롬프트 변경 |
| `change-header:STR` | 헤더 변경 |
| `transform-prompt:CMD` | 프롬프트 동적 변경 |
| `transform-header:CMD` | 헤더 동적 변경 |
| `transform-query:CMD` | 쿼리 동적 변경 |

### 기타

| 액션 | 설명 |
|------|------|
| `unbind:KEYS` | 바인딩 해제 |
| `rebind:KEYS` | 바인딩 복원 |
| `put:STR` | 쿼리에 문자열 삽입 |
| `print:STR` | 문자열 출력 (선택 대신) |
| `clear-screen` | 화면 지우기 |

## 플레이스홀더

| 플레이스홀더 | 의미 |
|--------------|------|
| `{}` | 현재 항목 (전체) |
| `{+}` | 선택된 항목들 |
| `{q}` | 현재 쿼리 |
| `{n}` | 현재 인덱스 (0부터) |
| `{+n}` | 선택된 인덱스들 |
| `{f}` | 현재 항목 (파일로) |
| `{+f}` | 선택된 항목들 (파일로) |
| `{1}` | 첫 번째 필드 |
| `{2..}` | 두 번째 이후 필드 |
| `{-1}` | 마지막 필드 |

### 필드 선택 예시

```bash
# 입력: "file.txt:10:hello world"
# --delimiter=: 로 구분

{1}   → file.txt
{2}   → 10
{3}   → hello world
{-1}  → hello world (마지막)
{2..} → 10:hello world

# 실제 사용
rg --line-number pattern |
  fzf --delimiter=: \
      --preview 'bat --highlight-line {2} {1}' \
      --bind 'enter:become(vim +{2} {1})'

# 인덱스 활용 (0부터)
seq 1 100 | fzf --bind 'enter:execute(echo "Item #{n}: {}")'
```

## 실전 패턴

### reload로 동적 업데이트

```bash
# 쿼리 변경 시 ripgrep 재실행
fzf --disabled \
    --bind 'start:reload:rg --color=always {q}' \
    --bind 'change:reload:rg --color=always {q} || true'
```

### 모드 전환

```bash
# CTRL-T로 검색 모드 토글
--bind 'ctrl-t:transform:[[ $FZF_PROMPT == "1. " ]] &&
  echo "change-prompt(2. )+disable-search" ||
  echo "change-prompt(1. )+enable-search"'
```

### 프리뷰 위치 순환

```bash
--bind 'ctrl-/:change-preview-window(down|hidden|right)'
```

### 다중 액션 체이닝

```bash
--bind 'enter:select-all+accept'
--bind 'ctrl-y:execute-silent(echo {} | pbcopy)+abort'
```

### 조건부 실행

```bash
# 선택 유무에 따라 다른 동작 (한 줄로 작성)
--bind 'enter:become([[ $FZF_SELECT_COUNT -eq 0 ]] && vim {1} || vim -q {+f})'

# 또는 변수로 분리 (06-recipes.md rfv 참조)
OPENER='[[ $FZF_SELECT_COUNT -eq 0 ]] && vim {1} || vim -q {+f}'
--bind "enter:become:$OPENER"
```
