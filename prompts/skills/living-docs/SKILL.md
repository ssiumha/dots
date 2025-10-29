---
name: living-docs
description: 프로젝트 지식베이스와 TODO를 관리합니다. 요구사항, 의사결정, 정책 문서를 업데이트하거나 작업 현황을 파악할 때 사용하세요.
---

# Living Docs

프로젝트의 지식과 실행 계획을 통합 관리하는 문서 시스템입니다.

## Instructions

### 프로젝트 자동 인식

**모든 워크플로우 시작 전에 다음 절차로 프로젝트를 확인합니다:**

1. **현재 경로 확인**
   ```bash
   pwd
   ```

2. **프로젝트명 추출**

   경로에 `~/repos/` 포함 시:
   - `~/repos/` 이후 첫 번째 디렉토리명 추출
   - 예: `~/repos/myapp1/src/components` → `myapp1`

   프로젝트명 정제 규칙:
   - `_slot` + 숫자 패턴 제거: `project_slot1` → `project`
   - 끝의 순수 숫자 제거: `myapp1` → `myapp`, `project2` → `project`
   - 나머지는 그대로 유지: `project_devops` → `project_devops`, `api_server` → `api_server`

   경로에 `~/repos/` 없으면:
   - 사용자에게 "어느 프로젝트를 관리할까요?" 질문
   - 직접 프로젝트명 입력 받기

3. **문서 디렉토리 확인**
   - `~/docs/{project}/` 존재 여부 확인
   - 없으면: "{project} 프로젝트 문서 디렉토리를 생성할까요?" 제안
   - 생성 시: `mkdir -p ~/docs/{project}/{knowledge,decisions,todos}`

4. **사용자 확인**
   - "{project} 프로젝트에서 작업하시는 것 맞나요?"
   - 잘못 인식된 경우 사용자가 수정 가능

### 문서 위치
모든 문서는 `~/docs/{project}/` 아래에 저장됩니다:
- `knowledge/`: 지식 문서 (아키텍처, 보안, 요구사항 등)
- `decisions/`: 의사결정 기록
- `todos/`: 할 일 목록

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
   - [todo-002] IP 업데이트 (마감: 02/15, 우선순위: 높음)

   대기 중: 5개
   - [todo-007] 배포 스크립트 작성 (마감: 02/20)

   완료: 12개
   ```

### 워크플로우 5: 지식 탐색 (Wiki 스타일)

사용자가 "X 관련 정보", "X 문서 찾아줘", "이 문서와 관련된 문서는?" 요청 시:

#### 0. 구조 파악 (항상 첫 단계)

탐색 전 프로젝트 문서 구조를 파악합니다:

```bash
# 전체 구조 시각화
Bash: lsd --tree ~/docs/{project}

# 문서 통계 빠르게 파악
Bash: find ~/docs/{project} -name "*.md" | wc -l
```

구조 파악 결과:
- 어떤 카테고리가 있는지 (architecture, security, requirements 등)
- 문서가 얼마나 있는지
- 어디를 중점적으로 탐색할지 판단

#### 1. 인덱스 탐색 (빠르고 넓게)

**목표**: 최소한의 Read로 관련 문서 후보군 파악

```bash
# 전략 A: Frontmatter에서 빠르게 스캔
# - 문서 ID, 제목, 태그만 추출하여 인덱스처럼 활용
Grep -A 10 "^---$" ~/docs/{project}/**/*.md | head -50

# 전략 B: 키워드 검색 (본문 포함)
Grep -i "keyword" ~/docs/{project}/**/*.md

# 전략 C: 태그 기반 필터링
Grep "tags:.*security" ~/docs/{project}/**/*.md

# 전략 D: 문서 ID 패턴 검색
Grep "^id: know-arch" ~/docs/{project}/**/*.md
```

**핵심 원칙**:
- Read는 최소화 (비용 절약)
- rg/Grep으로 먼저 후보 좁히기
- 필요한 문서만 정확히 Read

#### 2. 검색 전략 선택

인덱스 탐색 결과를 바탕으로 전략 선택:

**전략 A: 키워드 검색** ("API 설계 관련 문서 찾아줘")
```bash
# 1단계: 인덱스 탐색으로 후보 파악
Grep -i "api" ~/docs/{project}/**/*.md | head -20

# 2단계: 필요한 문서만 Read
Read ~/docs/{project}/knowledge/architecture/api-design.md
```

**전략 B: 특정 문서의 연결 탐색** ("이 문서와 관련된 문서는?")
- Forward Links: 현재 문서가 참조하는 문서
  - 본문의 `[[링크]]` 추출
  - Frontmatter의 `references:`, `related:` 확인
- Backlinks: 현재 문서를 참조하는 문서
  ```bash
  # 본문에서 현재 문서를 참조하는 문서 찾기
  Grep "\[\[현재-문서-id\]\]" ~/docs/{project}/**/*.md

  # Frontmatter에서 현재 문서를 참조하는 문서 찾기
  Grep "- 현재-문서-id" ~/docs/{project}/**/*.md
  ```

**전략 C: 태그 기반 탐색** ("security 태그 문서 모두 보여줘")
```bash
Grep "tags:.*security" ~/docs/{project}/**/*.md
```

**전략 D: 상태 기반 필터링** ("진행 중인 TODO 보여줘")
```bash
# 상태별 필터링
Grep "status: in-progress" ~/docs/{project}/todos/*.md

# 우선순위별 필터링
Grep "priority: high" ~/docs/{project}/todos/*.md
```

#### 3. 상세 분석 (필요한 것만)

인덱스 탐색으로 좁힌 문서들만 Read로 상세 분석

#### 4. 종합 리포트

- 직접 관련된 문서들
- Forward Links (문서가 참조하는 문서들)
- Backlinks (문서를 참조하는 문서들)
- 관련 TODO
- 의사결정 히스토리
- 태그별 분류

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

### 워크플로우 7: 문서 병합 (중복 방지)

문서 생성 또는 업데이트 후, 유사한 문서가 있는지 확인하여 병합을 제안합니다.

#### 자동 중복 검사 (문서 생성/업데이트 시)

1. **유사 문서 탐색**
   ```bash
   # 1단계: 같은 카테고리 내 문서 목록
   Bash: lsd ~/docs/{project}/knowledge/{category}/

   # 2단계: 유사 키워드로 검색
   Grep -i "{핵심키워드}" ~/docs/{project}/knowledge/{category}/*.md

   # 3단계: 태그 기반 유사도 확인
   Grep "tags:.*{관련태그}" ~/docs/{project}/**/*.md
   ```

2. **유사도 판단**
   - 같은 카테고리 + 같은 주제 → 높은 유사도
   - 태그 50% 이상 겹침 → 중간 유사도
   - 본문에 같은 키워드 다수 등장 → 병합 고려

3. **사용자에게 제안**
   ```
   유사한 문서를 발견했습니다:

   1. know-security-ip-whitelist.md (생성: 2024-01-15)
      - 태그: security, networking, ip
      - 주제: IP 화이트리스트 정책

   2. know-security-firewall-rules.md (생성: 2024-03-10)
      - 태그: security, networking, firewall
      - 주제: 방화벽 규칙

   이 문서들과 병합하시겠습니까?
   [1] 새 문서로 유지
   [2] 문서 1과 병합
   [3] 문서 2와 병합
   [4] 모두 병합하여 새 문서 생성
   ```

4. **병합 실행**

   사용자가 병합 선택 시:

   **방법 A: 기존 문서에 통합**
   - 기존 문서를 Read
   - 새 내용을 적절한 섹션에 추가
   - 히스토리에 병합 기록
   - 새 문서 파일은 생성하지 않음

   **방법 B: 새 통합 문서 생성**
   - 두 문서의 내용을 합쳐 새 문서 생성
   - 기존 문서들은 아카이브 또는 삭제
   - 모든 참조를 새 문서로 업데이트

   ```markdown
   ## 히스토리

   ### YYYY-MM-DD: 문서 병합
   **변경 내용:**
   - [[구-문서-id]]의 내용을 통합
   - [구체적으로 어떤 섹션이 추가되었는지]

   **이유:**
   - 유사한 주제의 중복 방지
   - 정보 중앙화

   ---
   ```

5. **참조 업데이트**
   - 구 문서를 참조하던 모든 문서 검색
   ```bash
   Grep "\[\[구-문서-id\]\]" ~/docs/{project}/**/*.md
   ```
   - 각 문서의 참조를 새 문서 ID로 교체
   - Git 커밋에 "refactor: merge documents" 기록

### 워크플로우 8: Git 관리

모든 문서 변경 사항은 Git으로 추적합니다.

#### 기본 원칙

- **자동 커밋**: 문서 생성/수정/삭제 시 자동으로 커밋
- **명확한 메시지**: 변경 내용이 명확하게 드러나는 커밋 메시지
- **원자적 커밋**: 하나의 논리적 변경은 하나의 커밋
- **절대 금지**: `--no-verify`, `--force` 옵션 사용 금지

#### 커밋 타이밍

**즉시 커밋:**
- 새 문서 생성
- 문서 삭제
- 중요한 정책/설정 변경
- 문서 병합

**배치 커밋 가능:**
- 단순 오타 수정
- 포맷 정리
- 여러 문서의 태그 일괄 수정

#### 커밋 메시지 규칙

```bash
# 새 문서 생성
docs(knowledge): add security/ip-policy

# 문서 업데이트
docs(knowledge): update security/ip-policy - change staging IP

# 의사결정 기록
docs(decision): add aws-region selection

# TODO 생성
docs(todo): add ip-update task

# TODO 완료 및 통합
docs(knowledge): merge todo-ip-update into security/ip-policy

# 문서 병합
docs(refactor): merge ip-whitelist into firewall-rules

# 문서 삭제
docs(delete): remove obsolete todo-old-task
```

#### 워크플로우 통합

각 워크플로우의 마지막 단계에 Git 커밋 추가:

**워크플로우 1 (지식 문서 업데이트) 완료 후:**
```bash
Bash: cd ~/docs/{project} && git add knowledge/{category}/{topic}.md && git commit -m "docs(knowledge): update {category}/{topic} - {간단한 설명}"
```

**워크플로우 2 (의사결정 기록) 완료 후:**
```bash
Bash: cd ~/docs/{project} && git add decisions/{slug}.md && git commit -m "docs(decision): add {slug}"
```

**워크플로우 3 (TODO 생성) 완료 후:**
```bash
Bash: cd ~/docs/{project} && git add todos/{slug}.md && git commit -m "docs(todo): add {slug}"
```

**워크플로우 7 (문서 병합) 완료 후:**
```bash
Bash: cd ~/docs/{project} && git add . && git commit -m "docs(refactor): merge {old-doc} into {new-doc}"
```

#### Git 저장소 초기화

프로젝트 문서 디렉토리 생성 시 자동으로 Git 초기화:

```bash
# 디렉토리 생성
mkdir -p ~/docs/{project}/{knowledge,decisions,todos}

# Git 초기화 (저장소가 없는 경우)
cd ~/docs/{project}
if [ ! -d .git ]; then
  git init
  echo "# {project} Documentation" > README.md
  git add README.md
  git commit -m "docs: initialize documentation repository"
fi
```

#### 변경 이력 조회

사용자가 "X 문서 변경 이력", "누가 언제 수정했어?" 요청 시:

```bash
# 특정 문서의 변경 이력
Bash: cd ~/docs/{project} && git log --oneline -- knowledge/security/ip-policy.md

# 상세 변경 내역
Bash: cd ~/docs/{project} && git log -p -- knowledge/security/ip-policy.md

# 최근 변경 사항
Bash: cd ~/docs/{project} && git log --since="1 week ago" --oneline
```

### 워크플로우 9: TODO 상태 관리 및 완료 처리

#### A. 상태 변경

사용자가 "X 작업 시작", "X 작업 진행 중", "X 작업 완료" 요청 시:

1. **TODO 파일 확인**
   ```bash
   Read ~/docs/{project}/todos/{slug}.md
   ```

2. **상태 업데이트**
   - pending → in-progress: `status: in-progress` 설정
   - in-progress → done: `status: done` + `completed: YYYY-MM-DD` 설정
   - 필요 시 역방향도 가능 (in-progress → pending)

3. **히스토리 추가**
   ```markdown
   ### YYYY-MM-DD: 상태 변경 (pending → in-progress)
   **내용:**
   - 작업 시작
   - [진행 상황이나 메모]

   ---
   ```

#### B. 완료 후 처리

TODO가 `status: done`이 되면 사용자에게 선택지 제안:

**옵션 1: Knowledge로 통합 (권장)**
- TODO의 내용이 지식으로 남을 가치가 있는 경우
- 관련 knowledge 문서를 찾거나 생성
- TODO의 주요 내용/결과를 knowledge 문서에 추가
- knowledge 문서 히스토리에 기록:
  ```markdown
  ### YYYY-MM-DD: [[todo-id]] 작업 완료 반영
  **변경 내용:**
  - [TODO에서 수행한 작업 내용]

  **관련 작업:**
  - 완료된 TODO: [[todo-id]]

  ---
  ```
- TODO 파일 삭제

**옵션 2: 파일 삭제**
- 단순 작업으로 별도 기록이 필요 없는 경우
- TODO 파일을 바로 삭제
```bash
rm ~/docs/{project}/todos/{slug}.md
```

4. **사용자 확인**
   "작업이 완료되었습니다. 이 TODO를:"
   - [1] Knowledge 문서에 통합 후 삭제 (권장)
   - [2] 파일만 삭제

## 중요 원칙

1. **사용자 중심**: 항상 사용자에게 확인하고 대화하며 진행
2. **명확한 커뮤니케이션**: 변경 전 요약, 변경 후 확인
3. **일관성**: 모든 문서는 템플릿 구조 유지
4. **추적 가능성**: 히스토리와 참조를 통한 변경 추적
5. **단순함**: 복잡한 스크립트 대신 Claude의 판단 활용

## Examples

### Example 1: 지식 문서 업데이트
```
User: "myproject IP 정책 업데이트해줘"
Assistant:
1. Read ~/docs/myproject/knowledge/security/ip-policy.md
2. "어떤 내용을 업데이트할까요?"
User: "Staging IP를 198.51.100.3으로 변경했어"
Assistant:
3. "변경 이유를 알려주세요"
User: "CDN 통합 때문이야"
Assistant:
4. Edit frontmatter (updated: 오늘)
5. Edit 현재 상태 섹션
6. Edit 히스토리 추가
7. "IP 정책 문서를 다음과 같이 업데이트했습니다:
   - Staging IP: 198.51.100.3
   - 이유: CDN 통합
   추가로 업데이트할 내용이 있나요?"
```

### Example 2: TODO 관리
```
User: "이번 주 할 일 보여줘"
Assistant:
1. Glob ~/docs/*/todos/*.md
2. Read 각 파일 frontmatter
3. 분석 및 정렬
4. "이번 주 할 일입니다:

   긴급 (마감 임박):
   - [myproject] IP 화이트리스트 업데이트 (02/15)

   진행 중:
   - [project-a] API 문서화 (02/18)

   대기:
   - [myproject] SSL 인증서 갱신 (02/20)

   어떤 작업부터 시작할까요?"
```

### Example 3: 의사결정 기록
```
User: "AWS 서울 리전 선택 결정 기록해줘"
Assistant:
1. "좋습니다. 몇 가지 확인하겠습니다:"
2. "왜 서울 리전을 선택했나요?"
User: "레이턴시 최소화"
Assistant:
3. "어떤 대안을 고려했나요?"
User: "도쿄, 싱가포르"
Assistant:
4. "이 결정이 어떤 부분에 영향을 주나요?"
User: "IP 설정, 배포 스크립트"
Assistant:
5. Write ~/docs/myproject/decisions/aws-region.md
6. "의사결정 문서를 생성했습니다.
   영향받는 문서:
   - IP 관리 정책
   - 배포 아키텍처

   IP 설정 변경 작업을 TODO로 만들까요?"
```

## Technical Details

상세한 템플릿 구조와 frontmatter 형식은 `REFERENCE.md`를 참조하세요.
