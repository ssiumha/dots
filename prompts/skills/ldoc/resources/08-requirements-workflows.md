# Requirements Workflows

요구사항 문서 작성, 상태 변경, 검색/분석 워크플로우입니다.

## 워크플로우 12: 요구사항 문서 작성

사용자가 "X 요구사항 작성", "requirement 추가" 요청 시:

**0. 프로젝트 확인 (자동 수행)**

1. **템플릿 사용**
   `templates/requirements.md`를 기반으로 새 문서 생성

2. **EARS 패턴 결정**
   사용자와 대화하여 적절한 패턴 선택:
   - "이 요구사항은 조건이 있나요? (상태/이벤트/기능)"
   - "항상 적용되는 요구사항인가요?"
   - "오류/예외 상황 처리인가요?"

   **패턴 선택 가이드**: `resources/ears-guide.md` 참조

3. **요구사항 정보 수집**
   - "어떤 시스템/컴포넌트에 대한 요구사항인가요?"
   - "시스템이 어떻게 동작해야 하나요?"
   - "수용 기준은 무엇인가요?"
   - "관련 결정이나 문서가 있나요?"

4. **EARS 문장 작성**
   선택한 패턴에 따라 요구사항 문장 구성:
   - Ubiquitous: "The {system} shall {response}."
   - State-Driven: "While {precondition}, the {system} shall {response}."
   - Event-Driven: "When {trigger}, the {system} shall {response}."
   - Optional Feature: "Where {feature}, the {system} shall {response}."
   - Unwanted Behavior: "If {condition}, then the {system} shall {response}."

5. **문서 생성**
   ```bash
   Write ~/docs/{project}/requirements/{category}/{slug}.md
   ```

   Frontmatter:
   - `id`: req-{category}-{slug}
   - `ears-pattern`: 선택한 패턴
   - `system`: 대상 시스템명
   - `status`: draft
   - `category`: functional | non-functional | constraint | interface

6. **관련 문서 링크**
   - 관련 결정/지식 문서에 역참조 추가 제안
   - 관련 요구사항이 있으면 depends-on 설정

7. **Git 커밋**
   ```bash
   cd ~/docs/{project} && git add requirements/{category}/{slug}.md && git commit -m "docs(requirement): add {category}/{slug}"
   ```

8. **자동 건강도 체크**
   - 유사 요구사항 중복 확인
   - 충돌하는 요구사항 경고

---

## 워크플로우 13: 요구사항 상태 변경

사용자가 "X 요구사항 승인", "X 요구사항 구현 완료" 요청 시:

1. **요구사항 파일 확인 및 상태 업데이트**
   - draft → proposed: 검토 요청
   - proposed → approved: 승인
   - approved → implemented: 구현 완료
   - 모든 상태 → deprecated: 폐기

2. **히스토리 추가** (간결성 원칙: 3-5줄)

3. **연동 작업**
   - approved → implemented: 관련 `verified-by` TODO/테스트 상태 확인
   - deprecated: 영향받는 문서에 알림

4. **Git 커밋**
   ```bash
   cd ~/docs/{project} && git add requirements/{category}/{slug}.md && git commit -m "docs(requirement): update {slug} status to {status}"
   ```

---

## 워크플로우 14: 요구사항 검색 및 분석

사용자가 "요구사항 현황", "시스템별 요구사항", "미구현 요구사항" 요청 시:

1. **검색 기준 확인**
   - 시스템별: `system:` 필드로 필터링
   - 상태별: `status:` 필드로 필터링
   - 카테고리별: 디렉토리 또는 `category:` 필드
   - EARS 패턴별: `ears-pattern:` 필드

2. **Grep으로 필터링**
   ```bash
   # 상태별
   Grep "status: approved" ~/docs/{project}/requirements/**/*.md

   # 시스템별
   Grep "system: auth-service" ~/docs/{project}/requirements/**/*.md
   ```

3. **리포트 제공**
   ```
   요구사항 현황 ({project})

   상태별:
   - draft: 3개
   - proposed: 2개
   - approved: 5개 (구현 대기)
   - implemented: 12개
   - deprecated: 1개

   시스템별:
   - auth-service: 8개
   - payment-service: 6개

   우선순위별:
   - critical: 2개 (approved)
   - high: 4개
   ```
