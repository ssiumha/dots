# fzf 검색 문법

fzf의 검색 문법은 여러 토큰을 조합하여 정밀한 필터링을 가능하게 합니다.

## 기본 원칙

- 스페이스로 구분된 여러 토큰은 **AND** 조건
- `|`로 구분된 토큰은 **OR** 조건
- 토큰 순서는 결과에 영향 없음

## 토큰 유형

| 토큰 | 유형 | 동작 | 예시 |
|------|------|------|------|
| `sbtrkt` | fuzzy-match | 문자가 순서대로 포함 | `fzf` → "fuzzy finder" |
| `'wild` | exact-match | 정확히 포함 | `'test` → "test" 포함 |
| `'wild'` | exact-boundary | 단어 경계 매칭 | `'add'` → "add" (단어) |
| `^music` | prefix-exact | 시작 문자열 | `^src` → "src/..." |
| `.mp3$` | suffix-exact | 끝 문자열 | `.ts$` → "*.ts" |
| `!fire` | inverse-exact | 제외 | `!node_modules` |
| `!^music` | inverse-prefix | 시작이 아닌 것 | `!^.` → 숨김파일 제외 |
| `!.mp3$` | inverse-suffix | 끝이 아닌 것 | `!.test.ts$` |

## OR 연산자

```bash
# .js 또는 .ts 파일
\.js$ | \.ts$

# src 또는 lib 디렉토리
^src/ | ^lib/
```

## 정확 매칭 모드

`--exact` 또는 `-e` 플래그로 기본 fuzzy를 비활성화:

```bash
fzf --exact
```

이 모드에서 `'` 접두사는 반대로 fuzzy를 활성화합니다.

## 실전 예시

### 파일 검색
```bash
# TypeScript 파일 중 test 제외
.ts$ !.test. !.spec.

# src 아래 index 파일
^src/ index

# 컴포넌트 파일
component .tsx$
```

### Git 브랜치
```bash
# feature 브랜치만
^feature/

# main, master, develop 제외
!^main !^master !^develop
```

### 프로세스
```bash
# node 프로세스
node !grep

# 특정 포트
:3000 | :8080
```

## 팁

1. **스페이스 주의**: `git add`보다 `gitadd`가 더 좋은 결과
2. **약어 활용**: `gas` → "git add src" (단어 경계 점수 활용)
3. **정규식 아님**: fzf 문법은 정규식이 아닌 자체 문법

## 관련 옵션

| 옵션 | 설명 |
|------|------|
| `--exact`, `-e` | fuzzy 비활성화 |
| `--algo=v1` | v1 알고리즘 (더 빠름) |
| `--algo=v2` | v2 알고리즘 (기본, 더 정확) |
| `--case` | 대소문자 구분 모드 |
| `--nth=N..` | 특정 필드만 검색 |
