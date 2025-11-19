---
name: living-docs
description: 프로젝트 지식베이스와 TODO를 관리합니다. 요구사항, 의사결정, 정책 문서를 업데이트하거나 작업 현황을 파악할 때 사용하세요.
---

# Living Docs

프로젝트의 지식과 실행 계획을 통합 관리하는 문서 시스템입니다.

## Instructions

### 프로젝트 자동 인식

**모든 워크플로우 시작 전에 프로젝트를 자동으로 확인합니다:**

1. **현재 경로에서 프로젝트명 추출**
   - `~/repos/{project}/...` 패턴에서 `{project}` 추출
   - 프로젝트명 정제: `_slot숫자` 제거, 끝 숫자 제거
   - 없으면 사용자에게 질문

2. **문서 디렉토리 확인 및 생성**
   - `~/docs/{project}/` 확인
   - 없으면 생성 제안: `mkdir -p ~/docs/{project}/{knowledge,decisions,todos}`

3. **사용자 확인**: "{project} 프로젝트 맞나요?"

### 문서 위치 및 파일명 규칙

**디렉토리 구조:**

모든 문서는 `~/docs/{project}/` 아래에 저장됩니다:
- `knowledge/{category}/`: 지식 문서 (아키텍처, 보안, 요구사항 등)
- `decisions/`: 의사결정 기록
- `todos/`: 할 일 목록

**파일명 규칙:**

1. **Self-descriptive** - 경로와 파일명만 봐도 내용 파악 가능해야 함
   - ✅ 좋은 예: `knowledge/{category}/{descriptive-topic}.md`
   - ❌ 나쁜 예: `knowledge/ABC-001.md`, `doc-YYYY-MM-DD.md`, `temp-notes.md`

2. **명명 규칙**
   - **kebab-case** 사용 (소문자 + 하이픈)
   - **2-4 단어** 권장
   - **영문 권장** (한글도 가능하지만 링크 호환성 고려)
   - **구체적이고 명확한 이름** 사용

3. **경로 패턴**
   - knowledge: `{project}/knowledge/{category}/{topic}.md`
     - 예: `{proj}/knowledge/{category}/{topic}.md`
   - decisions: `{project}/decisions/{topic}.md`
     - 예: `{proj}/decisions/{topic}.md`
   - todos: `{project}/todos/{topic}.md`
     - 예: `{proj}/todos/{topic}.md`

### 워크플로우 1: 지식 문서 업데이트

사용자가 "X 문서 업데이트" 요청 시:

**0. 프로젝트 확인 (위 "프로젝트 자동 인식" 절차에 따라 자동 수행)**

1. **문서 확인**
   ```bash
   Read ~/docs/{project}/knowledge/{category}/{topic}.md
   ```

2. **업데이트 유형 판단**
   사용자 요청과 변경 내용을 보고 둘 중 하나 선택:

   **경로 A: 간단한 수정** (오타, 내용 추가, 포맷 정리 등)
   - "어떤 내용을 업데이트할까요?" 물어보기
   - 내용 수정
   - `updated:` 필드만 갱신 (오늘 날짜)
   - 히스토리 생략 가능
   - 변경 사항 요약 후 완료

   **경로 B: 중요한 변경** (정책 변경, 아키텍처 수정, 설정 값 변경 등)
   - "어떤 내용을 업데이트할까요?"
   - "변경 이유를 알려주세요"
   - "영향받는 부분이나 관련 문서가 있나요?"
   - 필요시 추가 정보 요청
   - `updated:` 필드 갱신
   - "## 현재 상태" 섹션 업데이트
   - "## 히스토리" 섹션 최상단에 엔트리 추가:
     ```markdown
     ### YYYY-MM-DD: [변경 제목]
     **변경 내용:**
     - [구체적 변경사항]

     **이유:**
     - [변경 이유]

     **영향:**
     - 관련 문서: [[doc-id]]
     - 관련 TODO: [[todo-id]]

     ---
     ```
   - 관련 문서에 역참조 추가 제안
   - 필요시 TODO 생성 제안

3. **확인**
   - 변경 사항을 사용자에게 요약
   - 추가 업데이트 필요 여부 확인

4. **Git 커밋**
   ```bash
   cd ~/docs/{project} && git add knowledge/{category}/{topic}.md && git commit -m "docs(knowledge): update {category}/{topic} - {변경 요약}"
   ```

5. **자동 건강도 체크** (워크플로우 10)
   - 문서 크기 확인 (300+ 줄 시 분할 제안)
   - 중복 가능성 확인 (태그 80%+ 중복 시 병합 제안)
   - 이슈 발견 시 즉시 알림 및 리팩토링 제안

### 워크플로우 2: 의사결정 기록

사용자가 "X 결정 기록해줘" 요청 시:

1. **템플릿 사용**
   `templates/decision.md`를 기반으로 새 문서 생성

2. **사용자와 대화**
   - "어떤 결정을 했나요?"
   - "이유는 무엇인가요?"
   - "어떤 대안을 고려했나요?"
   - "어떤 영향이 있나요?"

3. **문서 생성**
   ```bash
   Write ~/docs/{project}/decisions/{slug}.md
   ```

   Frontmatter:
   - `id`: dec-{slug}
   - `decided`: 오늘 날짜
   - `impacts`: 영향받는 문서 ID 배열 (사용자에게 확인)

4. **관련 문서 업데이트**
   - 영향받는 문서들의 `references:` 에 이 결정 추가
   - 필요시 TODO 생성 제안

5. **Git 커밋**
   ```bash
   cd ~/docs/{project} && git add decisions/{slug}.md && git commit -m "docs(decision): add {slug}"
   ```

6. **자동 건강도 체크** (워크플로우 10)
   - 유사 결정 문서 중복 확인
   - 이슈 발견 시 알림

### 워크플로우 3: TODO 생성 및 관리

사용자가 "X 작업 TODO 만들어줘" 요청 시:

1. **템플릿 사용**
   `templates/todo.md` 기반으로 생성

2. **정보 수집**
   - "작업 내용을 설명해주세요"
   - "완료 조건은 무엇인가요?"
   - "관련 문서나 결정이 있나요?"
   - "마감일이 있나요?"

3. **문서 생성**
   ```bash
   Write ~/docs/{project}/todos/{slug}.md
   ```

4. **관련 문서 링크**
   관련 지식/결정 문서에 역참조 추가

5. **Git 커밋**
   ```bash
   cd ~/docs/{project} && git add todos/{slug}.md && git commit -m "docs(todo): add {slug}"
   ```

6. **자동 건강도 체크** (워크플로우 10)
   - 중복 TODO 확인
   - 이슈 발견 시 알림

### 워크플로우 4: 현황 파악

사용자가 "뭐 해야해?", "TODO 목록", "이번 주 할 일" 요청 시:

1. **TODO 파일 검색**
   ```bash
   Glob ~/docs/{project}/todos/*.md
   ```

2. **각 파일 분석**
   - Read로 frontmatter만 빠르게 확인
   - `status:` 필드 확인 (pending, in-progress, done)

3. **필터링 및 정렬**
   - status != done인 것만 (완료된 작업 제외)
   - "이번 주" 요청 시: 이번 주 월요일~일요일 범위 deadline만
   - deadline이 가까운 순
   - priority 높은 것 우선

4. **사용자에게 리포트**
   ```
   현재 진행 중: 3개
   - [todo-002] {작업명} (마감: MM/DD, 우선순위: 높음)

   대기 중: 5개
   - [todo-007] {작업명} (마감: MM/DD)

   완료: 12개
   ```

### 워크플로우 5: 지식 탐색 (Wiki 스타일)

사용자가 "X 관련 정보", "X 문서 찾아줘", "이 문서와 관련된 문서는?" 요청 시:

**핵심 원칙**: Read 최소화, Grep으로 후보 좁히기

1. **구조 파악**: lsd --tree로 전체 구조 확인
2. **인덱스 탐색**: Grep으로 키워드/태그/ID 검색
3. **검색 전략 선택**:
   - 키워드 검색
   - 연결 탐색 (Forward Links/Backlinks)
   - 태그 기반 탐색
   - 상태 기반 필터링
4. **상세 분석**: 필요한 문서만 Read
5. **종합 리포트**: 관련 문서/TODO/의사결정 제공

**상세 패턴**: `resources/knowledge-search-patterns.md` 참조

### 워크플로우 6: 새 문서 생성

사용자가 "X 문서 만들어줘" 요청 시:

1. **문서 유형 확인**
   - 지식 문서? → `templates/knowledge.md`
   - 의사결정? → `templates/decision.md`
   - TODO? → `templates/todo.md`

2. **사용자와 대화**
   - 카테고리 확인 (architecture, security 등)
   - 초기 내용 수집

3. **템플릿 기반 생성**
   적절한 위치에 파일 생성

4. **Git 커밋**
   ```bash
   cd ~/docs/{project} && git add {문서경로} && git commit -m "docs({유형}): add {slug}"
   ```

5. **자동 건강도 체크** (워크플로우 10)
   - 문서 크기 및 중복 확인
   - 이슈 발견 시 즉시 알림

### 워크플로우 7: 문서 병합 (중복 방지)

문서 생성/업데이트 후 유사 문서 확인:

1. **유사 문서 탐색**: 같은 카테고리, 키워드, 태그로 검색
2. **유사도 판단**: 태그 50%+ 겹침 → 병합 고려
3. **사용자 제안**: 병합 옵션 제시 (기존 문서 통합 / 새 문서 생성)
4. **병합 실행**:
   - 기존 문서 통합 또는 새 문서 생성
   - 히스토리 기록 (간결성 원칙: 3-5줄)
   - 참조 업데이트 (Grep으로 검색)
   - Git 커밋

### 워크플로우 8: Git 관리

모든 문서 변경 사항은 Git으로 추적합니다 (글로벌 `~/dots/prompts/rules/commit.md` 규칙 준수).

#### Living Docs 커밋 메시지 포맷

```bash
docs(knowledge): add {category}/{topic}
docs(knowledge): update {category}/{topic} - {간단한 설명}
docs(decision): add {slug}
docs(todo): add {slug}
docs(knowledge): merge {todo-slug} into {category}/{topic}
docs(refactor): merge {old-doc} into {new-doc}
docs(delete): remove {slug}
```

#### Git 저장소 초기화

프로젝트 문서 디렉토리 생성 시 자동으로 Git 초기화 (저장소가 없는 경우)

### 워크플로우 9: TODO 상태 관리 및 완료 처리

#### A. 상태 변경

사용자가 "X 작업 시작", "X 작업 진행 중", "X 작업 완료" 요청 시:

1. **TODO 파일 확인 및 상태 업데이트**
   - pending → in-progress: `status: in-progress` 설정
   - in-progress → done: `status: done` + `completed: YYYY-MM-DD` 설정

2. **히스토리 추가** (간결성 원칙 준수: 3-5줄)

#### B. 완료 후 처리

TODO가 `status: done`이 되면 사용자에게 선택지 제안:

**옵션 1: Knowledge로 통합 (권장)**

1. Category 결정 (architecture, security, operations 등)
2. 관련 문서 찾기 (Grep, Glob로 검색)
3. 통합 가치 판단 (체크리스트)
4. 문서 업데이트 또는 신규 생성
5. TODO 파일 삭제

**상세 절차**: `resources/todo-completion-guide.md` 참조

**옵션 2: 파일만 삭제**
- 단순 작업으로 별도 기록 불필요 시

사용자 확인: "[1] Knowledge 통합 후 삭제 (권장) / [2] 파일만 삭제"

### 워크플로우 10: 문서 건강도 자동 체크

**실행 시점**:
- 워크플로우 1, 2, 3, 6 완료 후 자동 실행
- 사용자가 "찾기 힘들어", "중복", "문서 정리" 언급 시

**자동 체크 항목**:
- 문서 크기: 300+ 줄 → 분할 제안
- 중복 가능성: 태그 80%+ 중복 → 병합 제안
- 참조 건강도: 끊어진 링크, 고아 문서

**상세 로직**: `resources/health-check.md` 참조

## 중요 원칙

1. **사용자 중심**: 항상 사용자에게 확인하고 대화하며 진행
2. **명확한 커뮤니케이션**: 변경 전 요약, 변경 후 확인
3. **일관성**: 모든 문서는 템플릿 구조 유지
4. **추적 가능성**: 히스토리와 참조를 통한 변경 추적
5. **단순함**: 복잡한 스크립트 대신 Claude의 판단 활용
6. **자동 건강도 관리**: 문서 작성 중 자동으로 정리 필요성 판단
7. **간결성**: 각 섹션은 핵심만 간결하게 작성

### 문서 작성 분량 가이드

문서를 작성할 때 다음 분량 기준을 준수하세요:

**지식 문서 (knowledge/)**:
- **현재 상태**: 1-3문장 (설정값, 현황만)
- **개요**: 2-4문장 (배경, 필요성)
- **히스토리 항목**: 총 3-5줄
  - 변경 내용: 1-2줄
  - 이유: 1-2줄
  - 영향: 1줄

**의사결정 문서 (decisions/)**:
- **결정 내용**: 2-3문장
- **이유**: 3-4문장 (핵심 근거만)
- **대안당**: 2-3줄 (장단점 각 1줄)
- **영향**: 항목당 1줄

**TODO 문서 (todos/)**:
- **설명**: 2-3문장
- **완료 조건**: 체크리스트 (3-5개 항목)
- **상세 내용**: 필요 최소한만

## 문서 작성 안티패턴

문서가 장황해지는 것을 방지하기 위해 다음 패턴을 피하세요:

### ❌ 피해야 할 패턴

1. **히스토리 항목이 10줄 이상**
   - 변경 배경부터 장황하게 서술
   - "왜 이렇게 결정했는지"를 여러 문단으로 설명
   - 당연한 내용까지 반복 설명

2. **"이유" 섹션에 불필요한 배경 설명**
   - 프로젝트 전체 배경부터 설명
   - 모든 고려 사항을 나열
   - 결정과 무관한 컨텍스트 추가

3. **대안 분석에 과도한 상세**
   - 각 대안의 모든 세부사항 나열
   - 명확한 장단점을 반복 설명
   - 고려하지 않은 대안까지 문서화

4. **TODO 설명이 문서 수준**
   - 작업 배경을 길게 서술
   - 구현 방법을 미리 상세히 작성
   - 완료 조건을 여러 문단으로 설명

### ✅ 권장 패턴

1. **히스토리 항목: 3-5줄**
   ```markdown
   ### YYYY-MM-DD: {변경 제목}
   **변경 내용:** {old-value} → {new-value}
   **이유:** {변경 이유}
   **영향:** {영향받는 부분}
   ```

2. **이유: 핵심 근거만 3-4문장**
   ```markdown
   ## 이유
   {결정 근거 1}.
   {결정 근거 2}.
   {대안} 대비 {개선 효과}.
   ```

3. **대안: 장단점 각 1-2줄**
   ```markdown
   ## 고려한 대안
   1. **{대안명}**
      - 장점: {장점}
      - 단점: {단점}
   ```

4. **TODO: 핵심만**
   ```markdown
   ## 설명
   {작업 대상} {작업 내용}.
   {관련 작업}의 일부.
   ```

## Examples

### 지식 문서 업데이트
User: "{topic} 문서 업데이트" → 워크플로우 1 → 사용자 대화 → 히스토리 추가 (3-5줄) → Git 커밋

### TODO 현황 파악
User: "이번 주 할 일" → 워크플로우 4 → Glob + Read → 우선순위/마감일 정렬 → 리포트 제공

### 의사결정 기록
User: "{주제} 결정 기록" → 워크플로우 2 → 템플릿 기반 생성 → 사용자 대화 (이유, 대안, 영향) → 관련 문서 링크 → Git 커밋

### TODO 완료 및 통합
User: "{task} 완료" → 워크플로우 9 → 통합 제안 → Category 결정 → Knowledge 통합 → TODO 삭제

## Technical Details

상세한 템플릿 구조와 frontmatter 형식은 `REFERENCE.md`를 참조하세요.
