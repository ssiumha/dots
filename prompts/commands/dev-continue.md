---
description: 진행 중인 개발 작업 재개 (Dev Docs 워크플로우 2)
---

# Dev Continue

중단된 개발 작업을 재개합니다. 3개 문서를 읽어 컨텍스트를 복원합니다.

## Instructions

**dev-docs 스킬의 워크플로우 2 (작업 재개) 실행**

### 1. 프로젝트 확인

living-docs 스킬 방식으로 현재 프로젝트 확인

### 2. 진행 중인 작업 검색

```bash
ls ~/docs/dev/{project}/active/
```

### 3. 작업 선택

- **작업이 1개만 있으면**: 자동 선택
- **작업이 여러 개면**: 사용자에게 선택 요청
  ```
  진행 중인 작업이 여러 개 있습니다:
  1. feature-user-auth
  2. bugfix-login-timeout

  어떤 작업을 이어서 하시겠습니까?
  ```

### 4. 3개 문서 모두 읽기

```bash
Read ~/docs/dev/{project}/active/{task-name}/plan.md
Read ~/docs/dev/{project}/active/{task-name}/context.md
Read ~/docs/dev/{project}/active/{task-name}/tasks.md
```

**중요**: 3개 문서를 모두 읽어야 합니다. 순서는 상관없습니다.

### 5. 타임스탬프 갱신

각 문서의 frontmatter `updated:` 필드를 오늘 날짜 (YYYY-MM-DD)로 갱신:

```markdown
---
task: {task-name}
updated: {오늘 날짜}
---
```

### 6. 사용자에게 요약

읽은 내용을 바탕으로 요약 제공:

```
🔄 작업을 이어갑니다: {task-name}

🎯 목표: {plan.md의 목표}

📊 진행 상황:
   - 전체: {tasks.md 통계}
   - 완료: {tasks.md 통계}
   - 진행률: {tasks.md 통계}

🚧 블로커:
   {tasks.md의 블로커가 있으면 표시}

📝 다음 단계:
   1. {tasks.md의 다음 단계}
   2. {tasks.md의 다음 단계}

💡 주요 컨텍스트:
   - {context.md의 핵심 내용 요약}
```

## 참고

- 상세 워크플로우: dev-docs SKILL.md 워크플로우 2
- 타임스탬프 규칙: dev-docs REFERENCE.md
