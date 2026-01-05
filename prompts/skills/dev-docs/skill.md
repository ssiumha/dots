---
name: dev-docs
description: 대형 작업과 복잡한 기능 개발 시 컨텍스트를 유지합니다. 계획 수립, 작업 시작, 작업 재개, 진행 상황 파악, 작업 완료 시 사용하세요.
---

# Dev Docs

대형/복잡한 작업의 컨텍스트를 유지하여 Claude의 "기억상실증"을 방지하는 시스템입니다.

## Instructions

### 전제 조건

모든 워크플로우는 자동으로 프로젝트를 확인합니다 (ldoc 스킬 참조).

### 문서 위치

`~/docs/dev/{project}/active/` 또는 `archive/`에 저장.
각 작업은 3개 문서 포함: plan.md, context.md, tasks.md

### 워크플로우 1: 작업 시작

플랜 모드를 종료하고 승인된 계획이 있을 때:

1. **작업명 결정**
   - kebab-case 형식 (예: feature-user-auth, bugfix-login-timeout)

1.5. **Living Docs 관련 지식 확인 (선택적)**

   작업을 시작하기 전에 프로젝트 지식베이스에서 관련 정보를 검색합니다.

   **사용자 확인:**
   ```
   작업을 시작하기 전에 관련 문서를 확인하시겠습니까?
   [1] 예 (권장, 중복 작업 방지 및 기존 패턴 참고)
   [2] 아니오 (바로 시작)
   ```

   **사용자가 '예' 선택 시:**

   ldoc 스킬의 **워크플로우 5 (지식 탐색)** 실행:

   1. **키워드 추출**: 작업명에서 검색어 추출
      - `feature-user-auth` → `user`, `auth`, `authentication`, `session`

   2. **관련 문서 검색**: ldoc Workflow 5 로직 활용
      - 구조 파악 (lsd --tree)
      - 빠른 인덱스 스캔 (Grep 우선, 최소 Read)
      - 관련 문서만 선택적 Read (상위 3-5개)

   3. **결과 요약**: 카테고리별 정리
      - 의사결정 (Decisions)
      - 아키텍처 (Architecture)
      - 보안/운영 (Security/Operations)
      - 관련 TODO (중복 확인)

   4. **권장 사항 제안**:
      - context.md에 추가할 관련 문서
      - 따라야 할 기존 결정/패턴
      - 조율 필요한 진행 중 작업

   **사용자가 '아니오' 선택 시:**
   - 검색 건너뛰고 바로 Step 2로 진행

   **Edge Cases:**
   - `~/docs/{project}` 없으면 자동 건너뛰기
   - 검색 결과 없으면 안내 후 진행

2. **작업 디렉토리 생성**
   ```bash
   mkdir -p ~/docs/dev/{project}/active/{task-name}
   ```

3. **3개 문서 생성**

   **plan.md**: 승인된 계획
   - 플랜 모드 계획 복사 + Frontmatter (task, created, updated, status)
   - 주요 섹션: 목표, 단계, 리스크, 성공 기준

   **context.md**: 핵심 컨텍스트 (`templates/context.md` 기반)
   - 주요 섹션: 핵심 파일, 아키텍처 노트, 의사결정, 관련 문서, 주의사항

   **tasks.md**: 체크리스트 (`templates/tasks.md` 기반)
   - 주요 섹션: 진행 상황, 블로커, 다음 단계

   (템플릿 전체 구조는 `REFERENCE.md` 또는 `templates/` 디렉토리 참조)

4. **사용자에게 확인**

   작업 시작 확인 및 위치 안내

### 워크플로우 2: 작업 재개

사용자가 "작업 이어서", "dev-docs 이어서", "뭐하고 있었지?" 요청 시:

1. **진행 중인 작업 확인**
   ```bash
   ls ~/docs/dev/{project}/active/
   ```

2. **작업 선택**

   작업이 1개면 자동 선택, 여러 개면 사용자에게 선택 요청

3. **3개 문서 모두 읽기**
   ```bash
   Read ~/docs/dev/{project}/active/{task-name}/plan.md
   Read ~/docs/dev/{project}/active/{task-name}/context.md
   Read ~/docs/dev/{project}/active/{task-name}/tasks.md
   ```

4. **타임스탬프 갱신**
   - 각 문서의 `updated:` 필드를 오늘 날짜로 갱신

5. **사용자에게 요약**

   작업명, 목표, 진행 상황, 다음 단계 안내

### 워크플로우 3: 현황 파악

사용자가 "진행 중인 작업", "뭐 하고 있어?", "작업 목록" 요청 시:

1. **Active 작업 검색**
   ```bash
   Glob ~/docs/dev/{project}/active/*/plan.md
   ```

2. **각 작업 분석**
   - 각 plan.md의 frontmatter만 빠르게 확인
   - `status:`, `created:`, `updated:` 필드 추출

3. **사용자에게 리포트**

   작업 목록(작업명, 생성일, 상태) 출력 및 선택 요청

### 워크플로우 4: 작업 완료 및 Living Docs 통합

사용자가 "작업 완료", "dev-docs 아카이브", "이 작업 끝났어" 요청 시:

1. **작업 확인**
   - 현재 작업 중인 task-name 확인
   - 또는 사용자에게 "어떤 작업을 완료하셨나요?" 질문

2. **최종 업데이트**
   - plan.md의 `status: completed`, `completed: YYYY-MM-DD` 설정
   - tasks.md의 모든 체크박스 확인 (미완료 항목이 있으면 알림)

3. **Context 분석 및 통합 제안**

   Read context.md와 tasks.md를 분석하여 Living Docs로 승격할 지식 찾기:

   - **의사결정**: 다른 작업에 영향을 주는 결정 → `decisions/{slug}.md`
   - **아키텍처**: 재사용 가능한 패턴 → `knowledge/architecture/{topic}.md`
   - **주의사항**: 팀이 알아야 할 함정 → `knowledge/{category}/{topic}.md`
   - **완료 기록**: 중요한 마일스톤 → Living Docs 히스토리 섹션

   (상세한 체크리스트는 `REFERENCE.md` 참조)

4. **통합 제안 요약**

   발견된 내용을 카테고리별로 나열하고 사용자에게 선택지 제공:
   - [1] 모두 Living Docs에 기록 (권장)
   - [2] 선택적으로 기록
   - [3] 기록하지 않고 아카이브만

5. **Living Docs 통합 실행** (사용자 승인 시)

   ldoc 스킬로 각 항목 통합 (decisions/, knowledge/ 문서 생성/업데이트)

6. **아카이브로 이동**
   ```bash
   mv ~/docs/dev/{project}/active/{task-name} ~/docs/dev/{project}/archive/{task-name}
   ```

7. **완료 확인**

   아카이브 위치, 통합된 Living Docs 목록 안내

### 워크플로우 5: 작업 중 업데이트

작업 진행 중 문서 업데이트:

1. **Context 업데이트**: 핵심 파일, 아키텍처 노트, 의사결정, 주의사항 추가/수정
2. **Tasks 업데이트**: 체크박스 완료, 블로커 추가, 다음 단계 갱신
3. **Plan 수정** (드문 경우): "## 변경 이력" 섹션에 변경 내용과 이유 기록

모든 업데이트 시 `updated:` 타임스탬프 갱신

### 워크플로우 6: 작업 간 이동

작업 전환 시: 현재 작업 상태 저장 (WF5) → 새 작업 로드 (WF2)

## 중요 원칙

1. **3개 문서 필수**: plan, context, tasks를 항상 함께 유지
2. **타임스탬프 갱신**: 문서 읽기/수정 시 updated 필드 갱신
3. **완료 후 정리**: 아카이브 + ldoc 통합 검토

## Living Docs와의 관계

Dev Docs 완료 → Living Docs로 승격 (작업 컨텍스트 → 영구 지식)

## Examples

### Example 1: 작업 시작

```
User: "플랜 모드 끝났어. 이제 시작하자"
Assistant: 작업명을 정해주세요 (예: feature-auth, bugfix-login)
User: "feature-user-auth"
Assistant: (프로젝트 확인 → 디렉토리 생성 → 3개 문서 생성)
"작업을 시작합니다: feature-user-auth
위치: ~/docs/dev/myapp/active/feature-user-auth/"
```

### Example 2: 작업 재개

```
User: "dev-docs 이어서"
Assistant: (작업 검색 → 3개 문서 읽기 → 타임스탬프 갱신)
"작업을 이어갑니다: feature-user-auth
목표: 사용자 인증 시스템 구축
완료: 3/8, 다음 단계: JWT 토큰 구현"
```

### Example 3: 작업 완료 및 Living Docs 통합

```
User: "작업 끝났어"
Assistant: (문서 읽기 → plan.md 완료 처리 → context.md 분석)
"Living Docs로 영속화할 내용:
📋 의사결정 (2개), 🏗️ 아키텍처 (1개), ⚠️ 주의사항 (1개)
[1] 모두 기록 [2] 선택 [3] 아카이브만"
User: "1번"
Assistant: (ldoc 스킬로 decisions/ 및 knowledge/ 문서 생성/업데이트 → 아카이브 이동)
"작업 완료! 의사결정 2개, 아키텍처 1개 기록됨"
```

## Technical Details

상세한 템플릿 구조와 frontmatter 형식은 `REFERENCE.md`를 참조하세요.
