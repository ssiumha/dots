# 지식 탐색 패턴 (Wiki 스타일)

사용자가 "X 관련 정보", "X 문서 찾아줘", "이 문서와 관련된 문서는?" 요청 시 사용합니다.

## 0. 구조 파악 (항상 첫 단계)

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

## 1. 인덱스 탐색 (빠르고 넓게)

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

## 2. 검색 전략 선택

인덱스 탐색 결과를 바탕으로 전략 선택:

### 전략 A: 키워드 검색

**사용 시나리오**: "API 설계 관련 문서 찾아줘"

```bash
# 1단계: 인덱스 탐색으로 후보 파악
Grep -i "api" ~/docs/{project}/**/*.md | head -20

# 2단계: 필요한 문서만 Read
Read ~/docs/{project}/knowledge/architecture/api-design.md
```

### 전략 B: 특정 문서의 연결 탐색

**사용 시나리오**: "이 문서와 관련된 문서는?"

**Forward Links**: 현재 문서가 참조하는 문서
- 본문의 `[[링크]]` 추출
- Frontmatter의 `references:`, `related:` 확인

**Backlinks**: 현재 문서를 참조하는 문서

```bash
# 본문에서 현재 문서를 참조하는 문서 찾기
Grep "\[\[현재-문서-id\]\]" ~/docs/{project}/**/*.md

# Frontmatter에서 현재 문서를 참조하는 문서 찾기
Grep "- 현재-문서-id" ~/docs/{project}/**/*.md
```

### 전략 C: 태그 기반 탐색

**사용 시나리오**: "security 태그 문서 모두 보여줘"

```bash
Grep "tags:.*security" ~/docs/{project}/**/*.md
```

### 전략 D: 상태 기반 필터링

**사용 시나리오**: "진행 중인 TODO 보여줘"

```bash
# Active TODO만 (pending, in-progress)
Grep "status: in-progress" ~/docs/{project}/todos/*.md

# 우선순위별 필터링
Grep "priority: high" ~/docs/{project}/todos/*.md

# 완료된 TODO 검색 (completed 디렉토리)
Grep "status: done" ~/docs/{project}/todos/completed/**/*.md

# 특정 월 완료 작업
Glob ~/docs/{project}/todos/completed/2025-01/*.md
```

## 3. 상세 분석 (필요한 것만)

인덱스 탐색으로 좁힌 문서들만 Read로 상세 분석

## 4. 종합 리포트

사용자에게 제공할 정보:
- 직접 관련된 문서들
- Forward Links (문서가 참조하는 문서들)
- Backlinks (문서를 참조하는 문서들)
- 관련 TODO
- 의사결정 히스토리
- 태그별 분류
