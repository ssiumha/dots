---
description: 진행 중인 작업 목록 확인 (Dev Docs 워크플로우 3)
---

# Dev Status

현재 진행 중인 모든 개발 작업의 상태를 확인합니다.

## Instructions

**dev-docs 스킬의 워크플로우 3 (현황 파악) 실행**

### 1. 프로젝트 확인

living-docs 스킬 방식으로 현재 프로젝트 확인

### 2. Active 작업 검색

```bash
Glob ~/docs/dev/{project}/active/*/plan.md
```

이 명령으로 모든 진행 중인 작업의 plan.md 파일을 찾습니다.

### 3. 각 작업 분석

각 plan.md 파일의 frontmatter만 빠르게 확인:

```markdown
---
task: feature-user-auth
created: 2025-01-15
updated: 2025-01-20
status: in-progress
---
```

필요한 필드:
- `task`: 작업명
- `created`: 생성일
- `updated`: 마지막 수정일
- `status`: 상태 (in-progress | blocked | completed)
- `completed`: 완료일 (status가 completed일 때만)

### 4. 사용자에게 리포트

작업 목록을 표 형식으로 출력:

```
📋 진행 중인 작업 ({총 개수}개)

| 작업명                          | 상태        | 생성일     | 마지막 수정 |
|--------------------------------|------------|-----------|-----------|
| feature-user-auth              | in-progress | 2025-01-15 | 2025-01-20 |
| bugfix-login-timeout           | blocked     | 2025-01-18 | 2025-01-19 |
| refactor-api-rest-to-graphql   | in-progress | 2025-01-10 | 2025-01-21 |

💡 작업을 이어서 하려면: /dev-continue
```

### 5. 선택 요청 (선택사항)

사용자가 바로 작업을 선택하고 싶다면:

```
어떤 작업을 시작하시겠습니까? (번호 입력 또는 작업명)
```

## 참고

- 상세 워크플로우: dev-docs SKILL.md 워크플로우 3
- Frontmatter 스키마: dev-docs REFERENCE.md
