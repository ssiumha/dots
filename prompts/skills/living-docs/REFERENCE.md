# Living Docs - Technical Reference

## Frontmatter 필드 설명

### 공통 필드

- `id`: 문서 고유 식별자 (형식: `{type}-{slug}`)
- `type`: 문서 유형 (knowledge, decision, todo)
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
│   └── todos/
│       ├── ip-update.md
│       └── api-docs.md
└── another-project/
    └── ...
```

## 상태 전이 (TODO)

```
pending → in-progress → done
   ↑           ↓
   └───────────┘
```

## 날짜 형식

모든 날짜는 YYYY-MM-DD 형식을 사용합니다.

날짜 생성:
```bash
date +%Y-%m-%d
# Output: 2025-02-10
```
