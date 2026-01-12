---
name: claude-project-setup
description: Configures project quality standards and .claude/ structure. Use when initializing projects or improving code quality practices.
---

# Claude Project Setup

프로젝트 분석 후 코드 품질 원칙을 적용하고 Claude 설정을 제안하는 스킬입니다.

**핵심 철학**:
- 스크립트로 만들 수 있는 건 스크립트로
- 원칙 중심 - 언어별 상세 구현은 실행 시 조사/작성
- 측정 → 개선 사이클

## 핵심 원칙

| 원칙 | 설명 | 측정 대상 |
|-----|------|----------|
| **품질 지표** | 측정하지 않으면 개선할 수 없다 | 빌드 오류, Lint 경고/오류, 오류 밀도 |
| **Tidy 커밋** | 커밋은 단일 목적, 원자적, 명확해야 | 파일 수, 변경 유형, 메시지 명확성 |
| **죽은 코드** | 사용하지 않는 코드는 부채 | 미사용 파일/export/dependency |
| **Claude 설정** | 프로젝트별 규칙은 rules/로 관리 | .claude/ 구조 완성도 |
| **자동화** | 반복 작업은 자동화 | hook, CI, 커맨드 존재 여부 |

**Tidy하지 않은 커밋 기준**: 파일 10개 초과, 변경 유형 혼합 (feat+fix), 모호한 메시지

---

## Instructions

### Phase 1: 프로젝트 분석

```bash
# 1. 기술 스택 감지
Glob package.json pyproject.toml Cargo.toml go.mod

# 2. 기존 Claude 설정 확인
Glob .claude/** CLAUDE.md

# 3. CI/CD 설정 확인
Glob .github/workflows/** Justfile Makefile

# 4. 품질 도구 설정 확인
Grep "eslint\|prettier\|ruff\|mypy\|clippy" package.json pyproject.toml
```

**확인 항목**: 기술 스택, Claude 설정 존재 여부, CI 파이프라인, 품질 도구

### Phase 2: 현황 진단

프로젝트에 맞는 명령어로 현황 측정:

| 언어 | 타입 체크 | Lint | 죽은 코드 |
|-----|----------|------|----------|
| TypeScript | `npx tsc --noEmit` | `npx eslint . --format json` | `npx knip` |
| Python | `mypy .` | `ruff check . --output-format json` | `vulture .` |
| Rust | `cargo check` | `cargo clippy` | - |
| Go | `go build ./...` | `golangci-lint run` | - |

**오류 밀도 계산**: 오류 수 / 코드 라인 수 (낮을수록 좋음)

### Phase 3: 제안 생성

원칙에 따라 제안 항목 생성:

| 원칙 | 체크 항목 | .claude/ 산출물 |
|-----|----------|----------------|
| 품질 지표 | 리포트 스크립트 없음 | `commands/quality-report.md` |
| Tidy 커밋 | 체크 스크립트 없음 | `commands/tidy-check.md` |
| 죽은 코드 | knip 등 미설정 | `commands/dead-code.md` |
| Claude 설정 | rules/ 미구성 | `rules/*.md` |
| 자동화 | hook 미설정 | `hooks/*.sh` |

### Phase 4: 대화형 적용

1. **제안 목록 제시** (AskUserQuestion)
2. **사용자 선택**
3. **선택된 항목 생성**
   - 실행 시 프로젝트에 맞는 스크립트 작성
   - rule-creator 스킬 연동하여 rules 생성

---

## .claude/ 구조 가이드라인

```
.claude/
├── CLAUDE.md             # 프로젝트 개요, 핵심 명령어, 아키텍처
├── CLAUDE.local.md       # 개인 설정 (gitignore)
├── rules/                # 조건부 규칙
│   ├── code-style.md     # 전역 코드 스타일
│   ├── testing.md        # 테스트 작성 규칙
│   └── {paths별}.md      # 특정 경로용 규칙
├── commands/             # 커스텀 슬래시 커맨드
│   └── quality-check.md  # /quality-check 등
└── hooks/                # 자동화 훅
    └── post-edit.sh      # 편집 후 lint 등
```

---

## Examples

### 예시 1: TypeScript 프로젝트 셋업

**User**: "이 프로젝트에 Claude 설정 잡아줘"

**Flow**:
1. package.json 확인 → TypeScript + ESLint 감지
2. `npx tsc --noEmit` 실행 → 오류 12개
3. `npx eslint .` 실행 → 경고 45개
4. AskUserQuestion: "어떤 설정을 적용할까요?"
   - [1] 품질 리포트 커맨드
   - [2] TypeScript rules
   - [3] post-edit hook
5. 사용자 선택에 따라 생성

### 예시 2: Python 프로젝트 셋업

**User**: "품질 관리 설정해줘"

**Flow**:
1. pyproject.toml 확인 → ruff, mypy 감지
2. `ruff check .` 실행 → 오류 5개
3. `mypy .` 실행 → 오류 8개
4. AskUserQuestion: "어떤 설정을 적용할까요?"
5. 사용자 선택에 따라 생성

### 예시 3: tidy하지 않은 커밋 진단

**User**: "커밋 히스토리 체크해줘"

**Flow**:
1. 최근 20개 커밋 분석
2. tidy하지 않은 커밋 식별:
   - #15: "fix: multiple fixes" (12개 파일)
   - #8: "feat+fix: add feature and fix bug"
3. `/tidy-check` 커맨드 생성 제안

---

## Anti-Patterns

| 문제 | 해결 |
|-----|------|
| 모든 프로젝트에 동일 설정 복사 | 프로젝트 분석 후 맞춤 설정 |
| 품질 도구 없이 rules만 생성 | 도구 먼저 확인, 없으면 설치 제안 |
| 한 번에 모든 설정 적용 | 우선순위 정해서 점진적 적용 |
| 측정 없이 개선 시도 | Phase 2 진단 먼저 실행 |

---

## Technical Details

**연동 스킬**:
- **rule-creator**: rules/*.md 생성 시 연동
- **patterns-devops**: CI 품질 체크 설정 시 참조
- **cli-guidelines**: 커맨드 스크립트 작성 시 참조
