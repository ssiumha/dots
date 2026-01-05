# Living Docs - Technical Reference

## Frontmatter 필드 설명

### 공통 필드

- `id`: 문서 고유 식별자 (형식: `{type}-{slug}`)
- `type`: 문서 유형 (knowledge, decision, todo, requirement)
- `created`: 생성 날짜 (YYYY-MM-DD)
- `updated`: 최종 수정 날짜 (YYYY-MM-DD)
- `tags`: 검색용 태그 배열

### Knowledge 문서

- `references`: 참조하는 다른 문서 ID 배열
  ```yaml
  references:
    - dec-aws-region
    - arch-deployment
  ```

### Decision 문서

- `decided`: 결정한 날짜
- `impacts`: 이 결정이 영향을 주는 문서 ID 배열

### TODO 문서

- `status`: pending | in-progress | done
- `priority`: low | medium | high | urgent
- `deadline`: 마감일 (YYYY-MM-DD)
- `depends-on`: 선행 작업 ID 배열
- `related`: 관련 지식/결정 문서 ID 배열
- `assignee`: 담당자
- `completed`: 완료 날짜 (status=done일 때)

### Requirement 문서

- `ears-pattern`: EARS 패턴 유형
  - ubiquitous | state-driven | event-driven | optional-feature | unwanted-behavior | complex
- `system`: 대상 시스템/컴포넌트명
- `status`: 요구사항 상태
  - draft | proposed | approved | implemented | deprecated
- `priority`: 우선순위 (low | medium | high | critical)
- `category`: 요구사항 분류
  - functional | non-functional | constraint | interface
- `depends-on`: 선행 요구사항 ID 배열
- `verified-by`: 검증 TODO/테스트 ID 배열

## 문서 ID 생성 규칙

### Knowledge 문서
- 형식: `know-{카테고리}-{주제}`
- 파일 경로: `knowledge/{카테고리}/{주제}.md`
- 예시:
  - ID: `know-security-ip-policy` → 파일: `knowledge/security/ip-policy.md`
  - ID: `know-architecture-database` → 파일: `knowledge/architecture/database.md`
  - ID: `know-requirements-user-features` → 파일: `knowledge/requirements/user-features.md`

### Decision 문서
- 형식: `dec-{slug}`
- 파일 경로: `decisions/{slug}.md`
- 예시:
  - ID: `dec-aws-region` → 파일: `decisions/aws-region.md`
  - ID: `dec-tech-stack` → 파일: `decisions/tech-stack.md`

### TODO 문서
- 형식: `todo-{slug}`
- 파일 경로: `todos/{slug}.md`
- 예시:
  - ID: `todo-ip-update` → 파일: `todos/ip-update.md`
  - ID: `todo-api-docs` → 파일: `todos/api-docs.md`

### Requirement 문서
- 형식: `req-{category}-{slug}`
- 파일 경로: `requirements/{category}/{slug}.md`
- 예시:
  - ID: `req-functional-user-login` → 파일: `requirements/functional/user-login.md`
  - ID: `req-security-password-encryption` → 파일: `requirements/security/password-encryption.md`
  - ID: `req-performance-api-response` → 파일: `requirements/performance/api-response.md`

**Slug 규칙:**
- kebab-case 사용 (소문자, 하이픈으로 연결)
- 영문 권장 (한글도 가능하지만 링크 호환성 고려)
- 간결하고 의미 있는 이름

## 문서 링크 형식

### 본문에서 참조
Markdown 내에서 다른 문서를 참조할 때:
```markdown
[[doc-id]]
[[dec-aws-region]]
[[todo-ip-update]]
[[know-security-ip-policy]]
```

### Frontmatter에서 참조
```yaml
# Knowledge 문서
references:
  - dec-aws-region
  - know-architecture-api-design

# Decision 문서
impacts:
  - know-security-ip-policy
  - know-architecture-deployment

# TODO 문서
related:
  - know-security-ip-policy
  - dec-aws-region
```

## 문서 간 연결

### Forward Links (참조)
- 본문: `[[링크]]` 사용
- Frontmatter: `references:`, `impacts:`, `related:` 배열

### Backlinks (역참조)
특정 문서를 참조하는 모든 문서 찾기:

1. **본문에서 참조**:
   ```bash
   Grep "\[\[현재-문서-id\]\]" ~/docs/{project}/**/*.md
   ```

2. **Frontmatter에서 참조**:
   ```bash
   Grep "- 현재-문서-id" ~/docs/{project}/**/*.md
   ```

### 사용 시나리오
- **지식 탐색**: "이 문서와 관련된 다른 문서는?"
- **영향 분석**: "이 결정이 어떤 문서에 영향을 주나?"
- **작업 추적**: "이 문서와 관련된 TODO는?"

## 디렉토리 구조 예시

```
~/docs/
├── myproject/
│   ├── knowledge/
│   │   ├── architecture/
│   │   │   ├── database.md
│   │   │   └── api-design.md
│   │   ├── security/
│   │   │   ├── ip-policy.md
│   │   │   └── auth-policy.md
│   │   └── requirements/
│   │       └── user-features.md
│   ├── decisions/
│   │   ├── aws-region.md
│   │   └── tech-stack.md
│   ├── todos/
│   │   ├── ip-update.md          # status: pending/in-progress
│   │   ├── api-docs.md            # status: pending/in-progress
│   │   └── completed/             # 완료된 TODO 보관
│   │       ├── 2025-01/
│   │       │   ├── auth-setup.md
│   │       │   └── db-migration.md
│   │       └── 2025-02/
│   │           └── feature-x.md
│   └── requirements/              # 요구사항 문서 (EARS 패턴)
│       ├── functional/
│       │   ├── user-login.md
│       │   └── user-registration.md
│       ├── non-functional/
│       │   └── api-response-time.md
│       ├── security/
│       │   └── password-encryption.md
│       └── interface/
│           └── external-payment-api.md
└── another-project/
    └── ...
```

### TODO 상태별 위치

| 상태 | 위치 | 설명 |
|------|------|------|
| `pending`, `in-progress` | `todos/{slug}.md` | 진행 중인 작업 |
| `done` | `todos/completed/YYYY-MM/{slug}.md` | 완료된 작업 (월별 보관) |

## 상태 전이 (TODO)

```
pending → in-progress → done
   ↑           ↓
   └───────────┘
```

## 상태 전이 (Requirement)

```
draft → proposed → approved → implemented
                      ↓              ↓
                deprecated ←────────┘
```

| 상태 | 설명 | 전환 가능 |
|------|------|----------|
| draft | 초안 작성됨 | → proposed |
| proposed | 검토 요청됨 | → approved, → draft |
| approved | 승인됨, 구현 대기 | → implemented, → deprecated |
| implemented | 구현 완료 | → deprecated |
| deprecated | 폐기됨 | - |

## 날짜 형식

모든 날짜는 YYYY-MM-DD 형식을 사용합니다.

날짜 생성:
```bash
date +%Y-%m-%d
# Output: 2025-02-10
```

## 문서 위치 및 파일명 규칙

### 디렉토리 구조

모든 문서는 `~/docs/{project}/` 아래에 저장됩니다:
- `knowledge/{category}/`: 지식 문서 (아키텍처, 보안, 요구사항 등)
- `decisions/`: 의사결정 기록
- `todos/`: 할 일 목록
  - `completed/YYYY-MM/`: 완료된 TODO (월별)

### 파일명 규칙

**Self-descriptive** - 경로와 파일명만 봐도 내용 파악 가능해야 함
- ✅ 좋은 예: `knowledge/{category}/{descriptive-topic}.md`
- ❌ 나쁜 예: `knowledge/ABC-001.md`, `doc-YYYY-MM-DD.md`, `temp-notes.md`

**명명 규칙**:
- **kebab-case** 사용 (소문자 + 하이픈)
- **2-4 단어** 권장
- **영문 권장** (한글도 가능하지만 링크 호환성 고려)
- **구체적이고 명확한 이름** 사용

**경로 패턴**:
- knowledge: `{project}/knowledge/{category}/{topic}.md`
- decisions: `{project}/decisions/{topic}.md`
- todos: `{project}/todos/{topic}.md`
- completed: `{project}/todos/completed/YYYY-MM/{topic}.md`

## TODO 분할 정책

### 분할 기준

다음 경우 복수 TODO로 분할 권장:
- 3개 이상의 명확한 하위 작업
- 각 작업이 독립적으로 완료 가능
- 병렬로 진행 가능한 작업
- 서로 다른 담당자가 맡을 수 있는 작업

### Slug 명명 규칙

**원칙**:
- ✅ 의미 있는 이름 (내용 파악 가능)
- ❌ 번호 매기기 (`-1`, `-2`, `-3`)

**패턴**:
```
공통 접두사 + 구체적 작업명
```

**예시**:

| 요청 | 생성되는 TODO |
|------|--------------|
| "User, Post, Comment 리소스 구현" | `resource-user.md`<br>`resource-post.md`<br>`resource-comment.md` |
| "로그인, 회원가입, 비밀번호 찾기 구현" | `auth-login.md`<br>`auth-signup.md`<br>`auth-password-reset.md` |
| "검색, 필터, 정렬 기능 추가" | `feature-search.md`<br>`feature-filter.md`<br>`feature-sort.md` |

### 작업 간 관계 설정

#### 순차 작업
작업 순서가 있는 경우 `depends-on` 사용:

```yaml
# auth-login.md
depends-on: []

# auth-signup.md
depends-on: [todo-auth-login]

# auth-password-reset.md
depends-on: [todo-auth-signup]
```

#### 병렬 작업
동시 진행 가능한 경우 공통 태그만 사용:

```yaml
# resource-user.md
tags: [resource, api]
depends-on: []

# resource-post.md
tags: [resource, api]
depends-on: []

# resource-comment.md
tags: [resource, api]
depends-on: []
```

### 그룹 식별

관련 TODO 찾기:
```bash
# 파일명 패턴으로 검색
Glob ~/docs/{project}/todos/resource-*.md

# 태그로 검색
Grep "tags:.*resource" ~/docs/{project}/todos/*.md
```

## Living Docs 커밋 메시지 포맷

### Knowledge 문서
```bash
docs(knowledge): add {category}/{topic}
docs(knowledge): update {category}/{topic} - {간단한 설명}
docs(knowledge): integrate todo-{slug} into {category}/{topic}
docs(knowledge): add {category}/{topic} from todo-{slug}
```

### Decision 문서
```bash
docs(decision): add {slug}
```

### TODO 문서
```bash
# 단일 TODO
docs(todo): add {slug}

# 복수 TODO 분할
docs(todo): add {작업명} tasks (3개)
docs(todo): add resource tasks (user, post, comment)

# 완료 및 아카이브
docs(todo): complete {slug} - moved to completed/YYYY-MM
docs(todo): archive completed todos from YYYY-MM
```

### Requirement 문서
```bash
docs(requirement): add {category}/{slug}
docs(requirement): update {slug} - {간단한 설명}
docs(requirement): update {slug} status to {status}
docs(requirement): deprecate {slug}
```

### Refactoring
```bash
docs(refactor): merge {old-doc} into {new-doc}
docs(delete): remove {slug}
```
