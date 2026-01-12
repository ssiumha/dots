# fzf 옵션 레퍼런스

## Display 옵션

### 레이아웃

| 옵션 | 설명 | 예시 |
|------|------|------|
| `--height=HEIGHT[%]` | 전체 화면 대신 일부 사용 | `--height 40%` |
| `--min-height=HEIGHT` | 최소 높이 | `--min-height 10` |
| `--layout=LAYOUT` | 레이아웃 방향 | `--layout reverse` |
| `--reverse` | `--layout=reverse` 단축 | |
| `--border[=STYLE]` | 테두리 스타일 | `--border rounded` |
| `--margin=MARGIN` | 여백 | `--margin 1,2` |
| `--padding=PADDING` | 내부 여백 | `--padding 1` |

**레이아웃 값**: `default`, `reverse`, `reverse-list`

**테두리 스타일**: `rounded`, `sharp`, `bold`, `double`, `block`, `thinblock`, `horizontal`, `vertical`, `top`, `bottom`, `left`, `right`, `none`

### 색상

```bash
# 테마 프리셋
--color=dark
--color=light
--color=16
--color=bw

# 개별 색상
--color=fg:gray,bg:black,hl:cyan
--color=fg+:white,bg+:gray,hl+:cyan
--color=info:yellow,prompt:blue,pointer:magenta
--color=marker:green,spinner:yellow,header:blue
```

### 스타일 프리셋

```bash
--style=default   # 기본
--style=minimal   # 최소 UI
--style=full      # 모든 기능 표시
```

## Search 옵션

| 옵션 | 설명 | 기본값 |
|------|------|--------|
| `--exact`, `-e` | 정확 매칭 모드 | fuzzy |
| `--algo=v1\|v2` | 매칭 알고리즘 | v2 |
| `--case` | 대소문자 구분 | smart-case |
| `-i` | 대소문자 무시 | |
| `+i` | 대소문자 구분 | |
| `--literal` | 메타문자 해석 안함 | |
| `--nth=N[,..]` | 검색 대상 필드 | 전체 |
| `--with-nth=N[,..]` | 표시 대상 필드 | 전체 |
| `--delimiter=STR` | 필드 구분자 | AWK 기본 |

### 필드 지정 예시

```bash
# 2번째 필드만 검색
--nth=2

# 2번째 이후 전부
--nth=2..

# 1, 3번째 필드
--nth=1,3

# 콜론 구분, 첫번째 필드
--delimiter=: --nth=1
```

## Interface 옵션

| 옵션 | 설명 |
|------|------|
| `--multi`, `-m` | 다중 선택 활성화 |
| `--no-mouse` | 마우스 비활성화 |
| `--cycle` | 목록 순환 |
| `--wrap` | 긴 줄 줄바꿈 |
| `--keep-right` | 긴 줄 오른쪽 표시 |
| `--no-hscroll` | 수평 스크롤 비활성화 |
| `--filepath-word` | 파일 경로용 단어 정의 |
| `--jump-labels=CHARS` | 점프 라벨 문자 |

### 프롬프트

```bash
--prompt='> '      # 프롬프트 문자열
--pointer='▶'      # 선택 포인터
--marker='✓'       # 선택 마커
--header='헤더'    # 헤더 텍스트
--header-lines=N   # 입력의 처음 N줄을 헤더로
```

## Scripting 옵션

| 옵션 | 설명 |
|------|------|
| `--query=STR`, `-q` | 초기 쿼리 |
| `--select-1`, `-1` | 결과 1개면 자동 선택 |
| `--exit-0`, `-0` | 결과 없으면 즉시 종료 |
| `--filter=STR`, `-f` | 비대화형 필터 모드 |
| `--print-query` | 쿼리도 출력 |
| `--expect=KEYS` | 종료 키 출력 |
| `--read0` | NUL 구분 입력 |
| `--print0` | NUL 구분 출력 |
| `--sync` | 입력 완료까지 대기 |
| `--listen[=PORT]` | HTTP 서버 모드 |

### 스크립트용 패턴

```bash
# 결과 없으면 종료
selected=$(... | fzf --exit-0) || exit 1

# 하나면 자동 선택
selected=$(... | fzf --select-1)

# 쿼리 + 선택 둘 다 필요
fzf --print-query | { read query; read selection; }

# 어떤 키로 종료했는지
fzf --expect=ctrl-v,ctrl-x | { read key; read selection; }
```

## 입력 옵션

| 옵션 | 설명 |
|------|------|
| `--ansi` | ANSI 색상 코드 파싱 |
| `--tac` | 역순 표시 |
| `--disabled` | fzf 필터링 비활성화 |
| `--no-sort` | 정렬 안함 |
| `--track` | 현재 항목 추적 |
| `--tail=NUM` | 최근 N개만 유지 |

## Walker 옵션

fzf 내장 파일 탐색기 설정:

```bash
--walker=file,dir,follow,hidden
--walker-root=DIR
--walker-skip=.git,node_modules,target
```

## 히스토리/세션

| 옵션 | 설명 |
|------|------|
| `--history=FILE` | 쿼리 히스토리 파일 |
| `--history-size=N` | 히스토리 최대 항목 (기본 100) |

```bash
# 히스토리 활성화
fzf --history=~/.fzf_history --history-size=1000
```

## Info 스타일

```bash
--info=default    # 기본 (매칭/전체)
--info=inline     # 프롬프트 옆에 표시
--info=hidden     # 숨김
--info=inline-right  # 오른쪽 정렬
```

## 성능 튜닝

| 옵션 | 설명 |
|------|------|
| `--algo=v1` | 빠른 알고리즘 (정확도↓) |
| `--algo=v2` | 정확한 알고리즘 (기본) |
| `--sync` | 입력 완료까지 대기 |
| `--tail=N` | 최근 N개만 메모리 유지 |

```bash
# 대량 데이터
fzf --algo=v1 --sync --tail=100000
```
