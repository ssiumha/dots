---
name: rule-creator
description: Creates and manages Claude Code rules files. Use when creating .claude/rules/ files or applying conditional rules to specific files/situations.
---

# Rule Creator

Claude Code용 rules 파일을 생성하고 관리합니다. Rules는 CLAUDE.md와 동일한 우선순위로 로드되며, `paths`로 조건부 적용이 가능합니다.

## Key Components

### 파일 위치

| 위치 | 경로 | 용도 |
|------|------|------|
| 프로젝트 | `.claude/rules/{name}.md` | 현재 프로젝트에서만 |
| 사용자 | `~/.claude/rules/{name}.md` | 모든 프로젝트에서 |

### Frontmatter

| 필드 | 필수 | 설명 |
|------|:----:|------|
| `paths` | X | 조건부 적용 glob 패턴 |

`paths`가 없으면 모든 파일에 적용됩니다.

## CLAUDE.md vs Rules 선택 기준

| 상황 | 권장 |
|------|------|
| 모든 상황에 적용 | CLAUDE.md |
| 특정 파일/폴더에만 적용 | rules/ + paths |
| 규칙이 많아 모듈화 필요 | rules/ (주제별 분리) |
| 팀에서 특정 규칙만 공유 | rules/ (개별 파일) |

## Creation Process

### 1. 기존 규칙 확인

중복 방지를 위해 검색:
```bash
Glob ~/.claude/rules/*.md
Glob .claude/rules/*.md
```

유사 규칙 발견 시 사용자에게 확인:
- 기존 규칙에 추가 vs 신규 생성

### 2. 용도 확인

AskUserQuestion으로 확인:
- **적용 범위**: 모든 파일? 특정 파일만?
- **위치**: 프로젝트? 사용자 레벨?

### 3. CLAUDE.md vs Rules 결정

```
모든 상황에 적용?
  → YES: CLAUDE.md에 추가 권장
  → NO: rules/ 생성 (paths 지정)
```

### 4. 파일 생성

**조건부 규칙 (paths 지정)**:
```markdown
---
paths: src/api/**/*.ts
---

# API Development Rules

- 모든 엔드포인트에 입력 검증 필수
- 표준 에러 응답 형식 사용
```

**전역 규칙 (paths 없음)**:
```markdown
# Testing Rules

- 모든 새 기능에 테스트 필수
- 커버리지 80% 이상 유지
```

## Modification Process

기존 규칙 수정 시:

1. **대상 확인**: 기존 규칙 파일 Read
2. **수정 유형 파악**:
   - paths 변경 → frontmatter 수정
   - 내용 추가 → 규칙 항목 추가
   - 내용 삭제 → 해당 항목 제거
   - 파일 분리 → 새 규칙 파일 생성 후 기존 내용 이동
3. **Edit으로 수정**: 기존 구조 유지하며 변경

## Glob 패턴 예시

| 패턴 | 매칭 대상 | 용도 예시 |
|------|----------|----------|
| `**/*.ts` | 모든 TypeScript 파일 | TypeScript 코딩 규칙 |
| `**/*.py` | 모든 Python 파일 | Python 스타일 가이드 |
| `src/api/**/*` | src/api/ 하위 모든 파일 | API 개발 규칙 |
| `src/components/**/*.tsx` | 특정 디렉토리의 React | 컴포넌트 규칙 |
| `*.md` | 루트의 Markdown 파일 | 문서 작성 규칙 |
| `src/**/*.{ts,tsx}` | src/ 하위 TS/TSX 파일 | 프론트엔드 규칙 |
| `{src,lib}/**/*.ts` | 여러 디렉토리 조합 | 공통 TypeScript 규칙 |
| `tests/**/*.test.ts` | 테스트 파일만 | 테스트 작성 규칙 |

**복합 패턴 예시**:
```yaml
paths: src/**/*.{ts,tsx}, lib/**/*.ts, tests/**/*.test.ts
```

## 우선순위

로드 순서 (공식 문서 기준):
1. Enterprise policy (조직 수준)
2. Project memory (`./.claude/CLAUDE.md`)
3. Project rules (`./.claude/rules/`)
4. User memory (`~/.claude/CLAUDE.md`)
5. Project local memory (`./.claude/CLAUDE.local.md`)

**나중에 로드되는 규칙이 더 높은 우선순위를 가집니다.**

## 활용 사례별 Rules 템플릿

### API 개발 규칙
```markdown
---
paths: src/api/**/*.ts
---
# API Development Rules
- 모든 엔드포인트에 Zod 입력 검증 필수
- 표준 에러 응답 포맷 사용
- OpenAPI 문서화 주석 포함
- correlation ID로 요청 로깅
```

### 테스트 작성 규칙
```markdown
---
paths: **/*.test.ts, **/*.test.tsx
---
# Test Standards
- 테스트명: "should [action] when [condition]"
- 테스트당 assertion 하나 권장
- fixtures는 conftest 또는 __fixtures__/ 사용
```

### 보안 민감 코드
```markdown
---
paths: src/auth/**/*.*, src/payments/**/*.*
---
# Security-Critical Rules
- 민감 데이터 로깅 금지 (passwords, tokens, card numbers)
- 함수 경계에서 모든 입력 검증
- parameterized query 필수
```

### React 컴포넌트
```markdown
---
paths: src/components/**/*.tsx, src/hooks/**/*.ts
---
# React Rules
- 함수형 컴포넌트 사용
- 로직은 custom hook으로 분리
- Props 타입 명시
```

### DB 마이그레이션
```markdown
---
paths: prisma/migrations/**/, db/migrations/**/
---
# Migration Rules
- 롤백 방법 명시 필수
- 프로덕션 데이터 복사본에서 테스트
```

상세 사례는 `REFERENCE.md` 참조.

## 디렉토리 조직 패턴

### 주제별 (권장 - 소규모)
```
.claude/rules/
├── code-style.md      # 전역
├── testing.md         # 전역
├── security.md        # 전역
└── api-design.md      # paths: src/api/**/*
```

### 기술 계층별 (중규모)
```
.claude/rules/
├── frontend/
│   ├── react.md        # paths: src/components/**/*
│   └── styles.md       # paths: **/*.css
├── backend/
│   ├── api.md          # paths: src/api/**/*
│   └── database.md     # paths: src/db/**/*
└── general.md          # 전역 (paths 없음)
```

### 도메인별 (대규모)
```
.claude/rules/
├── user-management/
│   └── auth.md         # paths: src/domains/user/**/*
├── payments/
│   └── security.md     # paths: src/domains/payment/**/*
└── shared.md           # 전역
```

**참고**: 위 경로는 예시입니다. 실제 프로젝트 구조에 맞게 조정하세요:
- Next.js: `app/`, `pages/`, `components/`
- NestJS: `src/modules/`, `src/common/`
- Python: `src/`, `app/`, `tests/`
- 모노레포: `packages/*/src/`

## 중요 원칙

1. **paths는 정말 필요할 때만**: 전역 규칙은 paths 생략. 불필요한 paths는 복잡성만 증가
2. **한 파일에 한 주제만**: `api.md`에 테스트 규칙 혼합 금지. 주제별로 분리
3. **명확한 파일명**: 내용을 반영하는 이름 사용 (예: `react-components.md`, `api-security.md`)
4. **CLAUDE.md와 역할 분리**: 핵심 프로젝트 지침은 CLAUDE.md, 세부 규칙은 rules/
5. **팀 공유 고려**: rules/는 버전 관리되므로 팀원도 사용. 개인 설정은 `~/.claude/rules/`

## Examples

### 조건부 규칙 생성

**User**: "API 파일에만 적용되는 규칙 만들어줘"

**Flow**:
1. 적용 범위 확인 → `src/api/**/*.ts`
2. 위치 확인 → 프로젝트
3. `.claude/rules/api.md` 생성 (paths 지정)

### 전역 규칙 생성

**User**: "테스트 규칙 추가해줘"

**Flow**:
1. 적용 범위 확인 → 모든 파일
2. CLAUDE.md vs rules 결정 → 이미 많은 규칙이 있어 분리 필요
3. `.claude/rules/testing.md` 생성 (paths 없음)

### CLAUDE.md 추가 권장

**User**: "커밋 메시지 규칙 추가해줘"

**Flow**:
1. 적용 범위 확인 → 모든 상황
2. 이미 CLAUDE.md가 간결함 → CLAUDE.md에 추가 권장
3. 사용자 동의 시 CLAUDE.md 수정

## Anti-Patterns

| ❌ 문제 | ✅ 해결 |
|--------|--------|
| 모든 규칙에 paths 추가 | 전역 규칙은 paths 생략 |
| 한 파일에 여러 주제 혼합 | 주제별 파일 분리 |
| CLAUDE.md 내용과 중복 | 한 곳에만 작성, 참조로 대체 |
| 너무 많은 전역 rules/ 파일 | CLAUDE.md 사용 권장 |
| 모호한 파일명 (`rules1.md`) | 명확한 이름 (`api-security.md`) |
