---
name: quality-audit
description: Audits and improves project code quality. Use when checking lint errors, test coverage, dead code, or commit hygiene.
invocable: true
invocable_description: Runs quality audit and generates actionable report. Use to check lint errors, test coverage, dead code, and commit hygiene.
---

# Quality Audit

프로젝트 품질을 측정하고 개선 방향을 제안하는 스킬입니다.

**핵심 철학**:
- 측정하지 않으면 개선할 수 없다
- 스크립트로 만들 수 있는 건 스크립트로
- 점진적 개선 (한 번에 모든 것 X)

## 핵심 원칙

| 원칙 | 설명 | 측정 대상 |
|-----|------|----------|
| **품질 지표** | 수치로 현황 파악 | 빌드 오류, Lint 경고/오류, 오류 밀도 |
| **Tidy 커밋** | 커밋은 단일 목적, 원자적, 명확해야 | 파일 수, 변경 유형, 메시지 명확성 |
| **죽은 코드** | 사용하지 않는 코드는 부채 | 미사용 파일/export/dependency |
| **자동화** | 반복 측정은 자동화 | hook, CI, 커맨드 존재 여부 |

**Tidy하지 않은 커밋 기준**: 파일 10개 초과, 변경 유형 혼합 (feat+fix), 모호한 메시지

---

## Instructions

### Phase 1: 프로젝트 분석

```bash
# 1. 기술 스택 감지
Glob package.json pyproject.toml Cargo.toml go.mod

# 2. CI/CD 설정 확인
Glob .github/workflows/** Justfile Makefile

# 3. 품질 도구 설정 확인
Grep "eslint\|prettier\|ruff\|mypy\|clippy" package.json pyproject.toml
```

**확인 항목**: 기술 스택, CI 파이프라인, 품질 도구

### Phase 2: 현황 진단

프로젝트에 맞는 명령어로 현황 측정:

| 언어 | 타입 체크 | Lint | 죽은 코드 |
|-----|----------|------|----------|
| TypeScript | `npx tsc --noEmit` | `npx eslint . --format json` | `npx knip` |
| Python | `mypy .` | `ruff check . --output-format json` | `vulture .` |
| Rust | `cargo check` | `cargo clippy` | - |
| Go | `go build ./...` | `golangci-lint run` | - |

**오류 밀도 계산**: 오류 수 / 코드 라인 수 (낮을수록 좋음)

### Phase 3: 리포트 생성

측정 결과를 요약하여 제시:

```markdown
## Quality Audit Report

### 현황
- Lint 오류: XX개 (밀도: X.XX)
- 타입 오류: XX개
- 죽은 코드: XX개 파일

### Tidy 커밋 분석 (최근 20개)
- Tidy하지 않은 커밋: X개
  - #15: "fix: multiple fixes" (12개 파일)

### 권장 조치
1. [우선] Lint 오류 해결
2. [중간] 죽은 코드 제거
3. [낮음] 커밋 분리 개선
```

### Phase 4: 자동화 제안

개선이 필요한 영역에 대해 자동화 도구 제안:

| 영역 | 제안 |
|-----|------|
| 품질 리포트 | `.claude/commands/quality-report.md` |
| Tidy 체크 | `.claude/commands/tidy-check.md` |
| 죽은 코드 | `.claude/commands/dead-code.md` |
| 자동 체크 | `.claude/hooks/post-edit.sh` |

→ 사용자 승인 후 생성 (rule-creator 연동)

---

## Examples

### 예시 1: TypeScript 프로젝트 감사

**User**: "프로젝트 품질 체크해줘"

**Flow**:
1. package.json 확인 → TypeScript + ESLint 감지
2. `npx tsc --noEmit` 실행 → 오류 12개
3. `npx eslint .` 실행 → 경고 45개
4. 리포트 생성 + 권장 조치 제시
5. AskUserQuestion: "자동화 스크립트 생성할까요?"

### 예시 2: 커밋 히스토리 진단

**User**: "커밋 품질 체크해줘"

**Flow**:
1. 최근 20개 커밋 분석
2. tidy하지 않은 커밋 식별
3. `/tidy-check` 커맨드 생성 제안

---

## Anti-Patterns

| 문제 | 해결 |
|-----|------|
| 측정 없이 개선 시도 | Phase 2 진단 먼저 실행 |
| 한 번에 모든 문제 해결 | 우선순위 정해서 점진적 적용 |
| 품질 도구 없이 체크 | 도구 먼저 확인, 없으면 설치 제안 |

---

## Technical Details

**연동 스킬**:
- **rule-creator**: 자동화 스크립트 생성 시 연동
- **devops-local-ci**: CI 품질 체크 설정 시 참조
