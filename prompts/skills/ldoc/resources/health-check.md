# Documentation Health Check

ldoc 문서의 건강도를 자동으로 체크하고 리팩토링을 제안합니다.

## 실행 시점

**자동 실행**:
- 워크플로우 1-6 (문서 생성/수정) 완료 후
- 사용자가 "찾기 힘들어", "중복", "문서 정리" 언급 시

**검사 대상**:
```bash
~/docs/{project}/knowledge/**/*.md
~/docs/{project}/decisions/**/*.md
```

## 검사 기준

### 1. 파일 크기 분석

```bash
# 모든 문서의 라인 수 확인
find ~/docs/{project} -name "*.md" -exec wc -l {} \;
```

**임계값**:

| 라인 수 | 상태 | 액션 |
|---------|------|------|
| 0-199 | ✅ 정상 | - |
| 200-299 | ⚠️ 경고 | 모니터링 (성장 추이 관찰) |
| 300-499 | 🔶 주의 | 분할 권장 |
| 500+ | 🔴 위험 | 즉시 분할 필요 |

**분할 제안 예시**:
```markdown
⚠️ 큰 문서 발견:
- knowledge/api.md (523줄)

제안:
→ knowledge/api/rest-endpoints.md (REST API)
→ knowledge/api/graphql-schema.md (GraphQL)
→ knowledge/api/authentication.md (인증)
```

**판단 기준**:
- 10+ 개의 ## 섹션 → 각 섹션을 별도 파일로
- 주제별 그룹핑 가능 → 하위 디렉토리 생성

### 2. 중복 검사

#### 2.1 태그 기반 중복

```bash
# 태그별 문서 그룹핑
Grep "^tags:" ~/docs/{project}/**/*.md
```

**유사도 계산**:
```
태그 중복률 = (공통 태그 수 / 전체 태그 수) × 100
```

**임계값**:
- 80%+ 태그 중복 → 병합 강력 권장
- 60-79% → 병합 검토
- 40-59% → 크로스 레퍼런스 추가

**병합 제안 예시**:
```markdown
🔄 중복 가능성:
1. knowledge/security-ip-policy.md
2. knowledge/networking-firewall.md

태그 중복: 85% (security, networking, firewall)

제안:
→ knowledge/security/network-policies.md (통합)
  - IP whitelist 정책
  - Firewall 설정
  - 네트워크 보안 규칙
```

#### 2.2 키워드 기반 중복

같은 핵심 키워드가 여러 문서에 반복 등장:

```bash
# 키워드 빈도 분석
Grep -i "{keyword}" ~/docs/{project}/knowledge/**/*.md | wc -l
```

**판단**:
- 같은 개념 설명이 3+ 문서에 존재 → 공통 문서로 추출
- 예시 코드 블록 중복 → 재사용 가능한 예시 문서 생성

### 3. 참조 건강도

#### 3.1 끊어진 링크

```bash
# 모든 내부 링크 추출
Grep -o "\[\[.*\]\]" ~/docs/{project}/**/*.md

# 각 링크가 실제 문서를 가리키는지 확인
```

**검사**:
- `[[know-api-design]]` → `~/docs/{project}/knowledge/api-design.md` 존재 여부
- 파일 이동/삭제로 인한 깨진 링크 탐지

**제안**:
```markdown
🔗 끊어진 링크:
- decisions/auth-method.md: [[know-old-api]] → 삭제된 문서
  수정: [[know-api-rest]] 로 변경

- knowledge/deployment.md: [[dec-infra]] → 경로 변경됨
  수정: [[dec-infrastructure-setup]] 로 변경
```

#### 3.2 누락된 크로스 레퍼런스

문서에서 다른 문서를 언급하지만 링크가 없는 경우:

```bash
# "see", "참고", "refer to" 같은 키워드 검색
Grep -i "see.*\(api\|auth\|deploy\)" ~/docs/{project}/**/*.md | grep -v "\[\["
```

**제안**:
```markdown
📎 누락된 링크:
- knowledge/deployment.md (line 45)
  "HA architecture를 참고하세요"
  → [[know-infra-ha-architecture]] 추가 권장
```

#### 3.3 고아 문서 (Orphaned Documents)

어디에서도 참조되지 않는 문서:

```bash
# frontmatter references가 비어있는 문서
Grep "references: \[\]" ~/docs/{project}/**/*.md

# 역으로 다른 문서에서 이 문서를 링크하는지 확인
```

**판단**:
- 참조 0개 + 마지막 업데이트 3개월+ → 아카이브 후보
- 참조 0개 + 최근 생성 → 다른 문서에 링크 추가 제안

**제안**:
```markdown
📦 고아 문서:
- decisions/old-tech-stack.md
  생성: 2024-01-15
  마지막 수정: 2024-01-20
  참조: 0개

  제안:
  [1] 관련 문서에 링크 추가
  [2] 아카이브 (outdated 태그 추가)
  [3] 삭제
```

### 4. 카테고리 분석

#### 4.1 미분류 문서

knowledge/ 바로 아래 평면 구조로 있는 문서:

```bash
# 하위 디렉토리 없이 직접 배치된 문서
find ~/docs/{project}/knowledge -maxdepth 1 -name "*.md"
```

**제안**:
```markdown
📂 미분류 문서 (4개):
- knowledge/api-design.md
- knowledge/deployment-process.md
- knowledge/security-checklist.md
- knowledge/database-schema.md

제안 카테고리 구조:
→ knowledge/api/design.md
→ knowledge/operations/deployment.md
→ knowledge/security/checklist.md
→ knowledge/database/schema.md
```

#### 4.2 과다 문서 카테고리

한 카테고리에 10+ 문서:

```bash
# 카테고리별 문서 수 집계
find ~/docs/{project}/knowledge/* -type d | while read dir; do
  echo "$dir: $(find "$dir" -maxdepth 1 -name "*.md" | wc -l)"
done
```

**제안**:
```markdown
📊 과다 문서 카테고리:
- knowledge/infrastructure/ (12개 문서)

제안 하위 분류:
→ infrastructure/cloud/ (AWS, GCP 관련 3개)
→ infrastructure/networking/ (네트워크 설정 4개)
→ infrastructure/monitoring/ (모니터링 5개)
```

## 리포트 형식

### 전체 요약

```markdown
# Documentation Health Report - {project}
Generated: {YYYY-MM-DD}

## 📊 Overview
- Total documents: {count}
- Knowledge: {count}
- Decisions: {count}
- TODOs: {count}

## 🚨 Critical Issues ({count})

### 1. Large Documents (500+ lines)
- knowledge/api-comprehensive.md (678줄)
  → api/rest.md + api/graphql.md + api/auth.md

### 2. High Duplication
- security-ip-policy.md + networking-firewall.md (85% tag overlap)
  → security/network-policies.md

## ⚠️ Warnings ({count})

### 3. File Size Warnings (300-499 lines)
- knowledge/deployment-guide.md (387줄)
  → 모니터링 필요

### 4. Missing Cross-References
- deployment.md mentions "HA architecture" (line 45)
  → Add [[know-infra-ha-architecture]]

## 💡 Recommendations ({count})

### 5. Category Reorganization
- knowledge/ (4 flat files)
  → Suggest: api/, operations/, security/, database/

### 6. Orphaned Documents
- decisions/old-stack.md (last update: 3 months ago)
  → Archive or add context

## 🎯 Suggested Actions

Priority 1 (Critical):
1. Split api-comprehensive.md
2. Merge duplicate security docs

Priority 2 (High):
3. Fix broken links (2개)
4. Reorganize categories

Priority 3 (Medium):
5. Add missing cross-references (3개)
6. Review orphaned docs (1개)
```

### 즉시 제안 (자동 실행 시)

워크플로우 1-6 완료 후 즉시 표시:

```markdown
⚠️ 문서 정리가 필요할 수 있습니다:

🔴 즉시 조치:
- knowledge/api.md (523줄) → 분할 권장

🔶 검토 필요:
- security-ip-policy.md ↔ networking-firewall.md (중복 85%)
  → 병합 제안

정리하시겠습니까?
[1] 즉시 리팩토링 (권장)
[2] 나중에
[3] 무시
```

## 리팩토링 실행

### 1. 문서 분할

**예시: api.md (523줄) → 3개 파일**

1. 원본 읽기 및 분석
2. 섹션별 내용 추출
3. 새 파일 생성:
   ```
   knowledge/api/
   ├── rest-endpoints.md    (200줄)
   ├── graphql-schema.md    (180줄)
   └── authentication.md    (143줄)
   ```
4. 원본 파일 삭제 또는 인덱스로 변환:
   ```markdown
   # API Documentation

   - [[know-api-rest]]: REST API 엔드포인트
   - [[know-api-graphql]]: GraphQL 스키마
   - [[know-api-auth]]: API 인증
   ```

### 2. 문서 병합

**예시: 2개 중복 문서 → 1개 통합**

1. 두 문서 읽기
2. 태그 및 내용 병합
3. 중복 제거
4. 새 문서 생성:
   ```markdown
   ---
   id: know-security-network
   tags: [security, networking, firewall, ip-policy]
   references: [...]
   ---

   # Network Security Policies

   ## IP Whitelist
   {기존 security-ip-policy.md 내용}

   ## Firewall Rules
   {기존 networking-firewall.md 내용}
   ```
5. 원본 파일 삭제
6. 다른 문서에서 링크 업데이트

### 3. 크로스 레퍼런스 추가

**예시: 누락된 링크 자동 추가**

```bash
# deployment.md 수정
Edit deployment.md:
  old: "HA architecture를 참고하세요"
  new: "HA architecture([[know-infra-ha-architecture]])를 참고하세요"
```

### 4. 카테고리 재구성

**예시: 평면 구조 → 계층 구조**

```bash
# 디렉토리 생성
mkdir -p ~/docs/{project}/knowledge/api
mkdir -p ~/docs/{project}/knowledge/operations

# 파일 이동
mv ~/docs/{project}/knowledge/api-design.md \
   ~/docs/{project}/knowledge/api/design.md

mv ~/docs/{project}/knowledge/deployment-process.md \
   ~/docs/{project}/knowledge/operations/deployment.md
```

## 체크리스트

리팩토링 전 확인:

- [ ] 원본 백업 (Git commit 확인)
- [ ] 분할/병합 범위 명확
- [ ] 새 파일 ID 중복 확인
- [ ] 크로스 레퍼런스 누락 없음
- [ ] 리팩토링 후 Git commit

리팩토링 후:

- [ ] 모든 링크 정상 작동
- [ ] frontmatter 일관성
- [ ] 카테고리 구조 명확
- [ ] 변경 사항 문서화

## 예외 처리

### 분할 제외 대상

다음 경우 분할하지 않음:
- Tutorial 문서 (순차적 읽기 필요)
- 참조 문서 (한 곳에서 찾기 편함)
- 명시적으로 "comprehensive" 태그

### 병합 제외 대상

다음 경우 병합하지 않음:
- 다른 관점 (예: 개발 vs 운영)
- 다른 대상 독자
- 명시적으로 분리 이유 기록됨
