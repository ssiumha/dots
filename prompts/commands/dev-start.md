---
description: 새로운 개발 작업 시작 (Dev Docs 워크플로우 1)
---

# Dev Start

플랜 모드 종료 후 새로운 개발 작업을 시작합니다.

## Instructions

**dev-docs 스킬의 워크플로우 1 (작업 시작) 실행**

### 1. 작업명 입력 받기

사용자에게 작업명을 물어보세요:
- 형식: kebab-case
- 패턴: {type}-{scope}-{what}-{tech}
- Type 접두사: feature-, bugfix-, refactor-, perf-, docs-, test-, chore-
- 예시: feature-session-based-user-auth, bugfix-login-timeout-5xx-error

작업명이 kebab-case가 아니거나 너무 애매하면 더 구체적으로 제안하세요.

### 1.5. Living Docs 관련 문서 확인 (선택적)

사용자에게 관련 문서를 확인할지 물어보세요:
```
작업을 시작하기 전에 관련 문서를 확인하시겠습니까?
[1] 예 (권장, 중복 작업 방지 및 기존 패턴 참고)
[2] 아니오 (바로 시작)
```

**상세 로직**: dev-docs SKILL.md 워크플로우 1, Step 1.5 참조
- living-docs Workflow 5 (지식 탐색) 활용
- 키워드 기반 관련 문서 검색
- 의사결정, 아키텍처, 보안, TODO 확인

### 2. 프로젝트 확인

living-docs 스킬 방식으로 현재 프로젝트 확인:
- git root 확인
- .claude/project.yaml 또는 package.json 등에서 프로젝트명 추출
- 프로젝트명을 {project}로 사용

### 3. 작업 디렉토리 생성

```bash
mkdir -p ~/docs/dev/{project}/active/{task-name}
```

### 4. 3개 문서 생성

#### plan.md

플랜 모드에서 승인된 계획을 기반으로 작성:

```markdown
---
task: {task-name}
created: {오늘 날짜 YYYY-MM-DD}
updated: {오늘 날짜 YYYY-MM-DD}
status: in-progress
---

# {작업명}

## 목표

{플랜 모드 계획의 목표}

## 단계

{플랜 모드 계획의 단계들}

## 리스크

{플랜 모드 계획의 리스크}

## 성공 기준

{플랜 모드 계획의 성공 기준}
```

#### context.md

templates/context.md 기반으로 초기화:

```markdown
---
task: {task-name}
updated: {오늘 날짜 YYYY-MM-DD}
---

# Context: {작업명}

## 핵심 파일

- (작업하며 추가)

## 아키텍처 노트

(작업하며 추가)

## 의사결정

(작업하며 추가)

## 관련 문서

(작업하며 추가)

## 주의사항

(작업하며 추가)

## 메모

(작업하며 추가)
```

#### tasks.md

templates/tasks.md 기반으로 초기화:

```markdown
---
task: {task-name}
updated: {오늘 날짜 YYYY-MM-DD}
---

# Tasks: {작업명}

## 진행 상황

### Phase 1: {첫 단계명}
- [ ] {plan.md의 첫 단계를 task로 분해}

## 완료된 작업

(작업하며 추가)

## 블로커

(작업하며 추가)

## 다음 단계

1. {plan.md의 첫 단계}

## 통계

- 전체: {N}
- 완료: 0
- 진행률: 0%
```

### 5. 사용자 확인

작업 시작 안내:

```
✅ 작업을 시작합니다: {task-name}

📁 위치: ~/docs/dev/{project}/active/{task-name}/
   - plan.md (계획)
   - context.md (컨텍스트)
   - tasks.md (작업 목록)

📋 다음 단계: {plan.md의 첫 단계}
```

## 참고

- 상세 템플릿: dev-docs SKILL.md, REFERENCE.md
- 프로젝트 확인: living-docs 스킬
