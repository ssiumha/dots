---
name: ldoc
description: Manages project knowledge base and TODO tracking. Use when recording decisions (ADR), creating TODOs, updating knowledge docs, checking "what to do", or organizing project documentation.
---

# Living Docs (ldoc)

프로젝트의 지식과 실행 계획을 통합 관리하는 문서 시스템입니다.

## 핵심 철학

- **사용자 대화 우선**: 자동 생성보다 확인/질문 우선
- **간결성**: 히스토리 3-5줄, 핵심만
- **상위 탐색**: `docs/` 디렉토리를 현재 위치에서 위로 찾기
- **템플릿 일관성**: 모든 문서는 frontmatter + 구조화된 섹션

## Instructions

### 변수 정의

| 변수 | 설명 | 예시 |
|------|------|------|
| `{docs}` | 프로젝트 문서 디렉토리 (상위 탐색으로 찾은 `docs/`) | `~/pj/myproj/docs` |
| `{project}` | 프로젝트명 (`{docs}`의 부모 디렉토리명) | `myproj` |

### 트리거 매핑

| 사용자 요청 | 워크플로우 |
|-------------|-----------|
| "X 문서 업데이트", "X 수정" | WF1: 지식 문서 업데이트 |
| "X 결정 기록", "왜 X로 했는지" | WF2: 의사결정 기록 |
| "X TODO", "X 작업 만들어" | WF3: TODO 생성 |
| "뭐 해야해?", "TODO 목록", "이번 주 할 일" | WF4: 현황 파악 |
| "X 관련 정보", "X 문서 찾아줘" | WF5: 지식 탐색 |
| "X 문서 만들어" | WF6: 새 문서 생성 |
| "X 완료", "X 시작" | WF7: TODO 상태 변경 |
| "문서 정리", "중복 확인" | WF8: 건강도 체크 |

### 프로젝트 자동 인식 (모든 워크플로우 공통)

1. **docs 디렉토리 탐색**: 현재 디렉토리에서 상위로 올라가며 `docs/` 찾기
2. **없으면**: 사용자에게 프로젝트 루트 질문 → `mkdir -p {docs}/{knowledge,decisions,todos}`
3. **확인**: "{project} 프로젝트 맞나요?"

### 공통 절차

#### Git 커밋 (모든 변경 후)
```bash
git -C {docs} add {변경파일} && git -C {docs} commit -m "docs({유형}): {동작} {대상}"
```
유형: knowledge, decision, todo, requirement | 동작: add, update, archive, merge

#### 건강도 체크 (문서 생성/수정 후)
300줄+ → 분할 제안, 태그 80%+ 중복 → 병합 제안

---

## 핵심 워크플로우

### WF1: 지식 문서 업데이트

**경로 A (간단한 수정)**: 오타, 내용 추가
- `updated:` 필드 갱신
- 히스토리 생략 가능

**경로 B (중요한 변경)**: 정책, 아키텍처, 설정 변경
- 사용자에게 질문: 변경 내용, 이유, 영향
- `## 히스토리` 최상단에 추가 (3-5줄)
- 관련 문서에 역참조 추가 제안

### WF2: 의사결정 기록

1. 사용자와 대화: 결정 내용, 이유, 대안, 영향
2. `templates/decision.md` 기반 생성 → `{docs}/decisions/{slug}.md`
3. 영향받는 문서에 역참조 추가

### WF3: TODO 생성

1. **복수 작업 감지**: 쉼표, 접속사, 나열 패턴 확인
2. **분할 제안**: "[1] 각각 분할 (권장) / [2] 하나로 통합"
3. **관계 확인** (분할 시): "[1] 순차 (depends-on 체인) / [2] 병렬"
4. **정보 수집**: 내용, 완료 조건, 관련 문서, 마감일
5. `templates/todo.md` 기반 생성 → `{docs}/todos/{slug}.md`

Slug 규칙: `resources/07-todo-naming-guide.md`

### WF4: 현황 파악

```bash
Glob {docs}/todos/*.md  # completed/ 제외
```

출력 형식:
```
진행 중: N개
- [todo-xxx] {작업명} (마감: MM/DD)

대기 중: N개
- [todo-xxx] {작업명}
```

정렬: deadline 가까운 순 → priority 높은 순

### WF5: 지식 탐색

**원칙**: Read 최소화, Grep으로 후보 좁히기

```bash
# 1. 구조 파악
lsd --tree {docs}

# 2. 키워드/태그/ID 검색
Grep "태그명" {docs}/**/*.md
Grep "\[\[doc-id\]\]" {docs}/**/*.md  # backlinks
```

3. 필요한 문서만 Read → 관련 문서 리포트

### WF6: 새 문서 생성

1. 유형 확인 → 템플릿 선택:
   - 지식 → `templates/knowledge.md` → `{docs}/knowledge/{category}/{slug}.md`
   - 의사결정 → `templates/decision.md` → `{docs}/decisions/{slug}.md`
   - TODO → `templates/todo.md` → `{docs}/todos/{slug}.md`
2. 카테고리/내용 수집 후 생성

### WF7: TODO 상태 변경

**상태 전이**:
- pending → in-progress: `status: in-progress`
- in-progress → done: `status: done`, `completed: YYYY-MM-DD`

**완료 시**:
```bash
mkdir -p {docs}/todos/completed/YYYY-MM
mv {docs}/todos/{slug}.md {docs}/todos/completed/YYYY-MM/
```

사용자에게 선택: "[1] 이대로 보관 / [2] Knowledge로 통합 / [3] 삭제"

### WF8: 건강도 체크

수동 실행 또는 "문서 정리", "중복" 언급 시:
- 문서 크기 체크
- 중복 태그 분석
- 끊어진 링크 확인

상세: `resources/health-check.md`

---

## 추가 워크플로우

자주 사용하지 않는 워크플로우는 리소스 참조:

| 워크플로우 | 설명 | 참조 |
|-----------|------|------|
| 문서 병합 | 중복 문서 통합 | `REFERENCE.md` |
| TODO 아카이브 | 6개월+ completed 정리 | `resources/completed-archive-policy.md` |
| 요구사항 관리 | EARS 기반 작성/상태관리 | `resources/08-requirements-workflows.md` |

---

## 안티패턴

| 문제 | 해결 |
|------|------|
| 확인 없이 문서 생성 | 항상 프로젝트/내용 확인 먼저 |
| 히스토리 10줄 이상 | 3-5줄로 핵심만 |
| 문서 300줄 초과 | 분할 제안 |
| 태그 없는 문서 | frontmatter에 tags 필수 |
| 참조 없는 결정 | impacts 필드로 영향 문서 연결 |

## Examples

### 지식 문서 업데이트
```
User: "배포 프로세스 문서 업데이트해줘"
→ WF1 → 프로젝트 확인 → "어떤 내용?" → 경로 A/B 판단
→ 간단 수정: updated 갱신만
→ 중요 변경: 히스토리 추가 + 역참조
→ Git 커밋
```

### TODO 생성 (복수)
```
User: "User, Post API 구현 TODO"
→ WF3 → 복수 감지 → "[1] 분할 / [2] 통합" → 분할 선택
→ "[1] 순차 / [2] 병렬" → 병렬 선택
→ impl-user-api.md, impl-post-api.md 생성
→ 공통 태그: api, implementation
```

### 현황 파악
```
User: "뭐 해야해?"
→ WF4 → Glob todos/*.md → frontmatter 분석
→ 진행 중 2개, 대기 5개 리포트
```

## Technical Details

**CLI**: `ldoc` (현황), `ldoc list` (TODO), `ldoc health` (건강도)

**템플릿/Frontmatter**: `REFERENCE.md` 참조
