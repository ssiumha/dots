# fzf Reference

fzf 기반 스크립트/프로그램 작성 시 참조 문서입니다.

## Version Compatibility

**권장**: fzf 0.30+ (대부분의 기능 지원)

| 버전 | 추가된 기능 |
|------|-------------|
| 0.20+ | `--bind`, `execute`, `preview` |
| 0.24+ | `reload`, `change-prompt` |
| 0.27+ | `become`, `transform` |
| 0.30+ | `--walker`, `--walker-skip` |
| 0.35+ | `--gap`, `--marker-multi-line` |
| 0.38+ | `--listen`, HTTP 서버 모드 |
| 0.48+ | `--style` 프리셋 |

버전 확인: `fzf --version`

## 리소스 구조

| 파일 | 내용 | 용도 |
|------|------|------|
| `01-search-syntax.md` | 검색 문법 (fuzzy, exact, prefix 등) | 검색 패턴 설계 |
| `02-options.md` | 주요 옵션 (display, search, interface) | 옵션 조합 |
| `03-bindings.md` | --bind 액션 전체 목록 | 인터랙션 설계 |
| `04-preview.md` | 프리뷰 창 설정 | 컨텍스트 표시 |
| `05-shell-integration.md` | 쉘 통합 (환경변수, 키바인딩) | 쉘 설정 |
| `06-recipes.md` | 실용 레시피 (git, file, process 등) | 구현 참조 |
| `07-advanced-patterns.md` | 멀티라인, 프로그래밍 언어 통합 | 고급 사용 |

## 권장 학습 순서

1. **기본**: `01-search-syntax.md` → `02-options.md`
2. **인터랙션**: `03-bindings.md` → `04-preview.md`
3. **통합**: `05-shell-integration.md` → `06-recipes.md`
4. **고급**: `07-advanced-patterns.md`

## 핵심 패턴

### 기본 파이프라인

```bash
<source> | fzf [options] | <action>
```

### 표준 옵션 조합

```bash
fzf \
  --height 50% \
  --layout reverse \
  --border \
  --preview 'preview_cmd {}' \
  --preview-window 'right:50%:wrap' \
  --bind 'ctrl-/:toggle-preview'
```

### 함수 템플릿

```bash
my_fzf_function() {
  local selected
  selected=$(
    source_command |
    fzf --height 40% \
        --layout reverse \
        --preview 'preview_cmd {}'
  )
  [[ -n "$selected" ]] && action_command "$selected"
}
```

## 트러블슈팅

| 문제 | 원인 | 해결 |
|------|------|------|
| 프리뷰 안 보임 | 명령어 오류 | `--preview-window` 확인, 명령어 단독 테스트 |
| 색상 안 나옴 | ANSI 미파싱 | `--ansi` 추가 |
| 느림 | 소스 과다 | `fd` 사용 (find 대체), `--walker-skip` |
| 선택 안 됨 | 필드 문제 | `--nth`, `--delimiter` 확인 |

## 외부 도구 연동

| 도구 | 용도 | 설치 |
|------|------|------|
| `fd` | 빠른 파일 검색 | `brew install fd` |
| `bat` | 구문 강조 프리뷰 | `brew install bat` |
| `rg` (ripgrep) | 빠른 텍스트 검색 | `brew install ripgrep` |
| `delta` | Git diff 강조 | `brew install git-delta` |
| `eza` | 컬러 ls | `brew install eza` |
