# JD 기술 참조

Johnny.Decimal 체계 기술 사양

## 네이밍 규칙

### 디렉토리

| 레벨 | 형식 | 예시 |
|------|------|------|
| 영역(Area) | `{start}-{end}-{Name}` | `20-29-Architecture` |
| 카테고리(Category) | `{number}-{Name}` | `21-ADR` |
| 서브폴더 | kebab-case | `assets/`, `diagrams/` |

### 파일

```
{category}.{id}-{slug}.md
```

| 구성요소 | 설명 | 예시 |
|----------|------|------|
| category | 카테고리 번호 (2자리) | `21` |
| id | 순차 번호 (2자리) | `01`, `02` |
| slug | kebab-case 제목 | `database-selection` |

**예시:**
- `21.01-database-selection.md`
- `31.02-user-api.md`
- `63.01-server-outage.md`

## ID 체계

### 구조

```
{category}.{id}
```

- **category**: 10-99 범위의 2자리 숫자
- **id**: 00-99 범위의 2자리 숫자 (카테고리 내 고유)

### Standard Zeros (예약 ID)

| ID | 용도 | 설명 |
|----|------|------|
| `.00` | JDex | 카테고리 인덱스 |
| `.01` | Inbox | 미분류/임시 항목 |
| `.02` | Tasks | 작업/태스크 |
| `.03` | Templates | 템플릿 |
| `.04` | Links | 관련 링크 모음 |
| `.05-.07` | (예약) | 프로젝트별 사용 가능 |
| `.08` | Someday | 언젠가 정리할 항목 |
| `.09` | Archive | 카테고리 내 아카이브 |

**Note**: 일반 문서 ID는 `.10`부터 시작 권장 (00-09 System 영역 제외)

## Frontmatter 스키마

### 공통 필드

```yaml
---
id: "21.01"                    # JD ID (필수)
title: "Database Selection"    # 문서 제목 (필수)
status: draft                  # 문서 상태
date: 2024-01-15               # 생성일
updated: 2024-01-20            # 수정일
author: "@username"            # 작성자
tags: [database, postgresql]   # 태그
---
```

### 상태(status) 값

| 상태 | 설명 |
|------|------|
| `draft` | 작성 중 |
| `review` | 검토 중 |
| `approved` | 승인됨 |
| `active` | 활성 (운영 중) |
| `deprecated` | 폐기 예정 |
| `superseded` | 대체됨 (새 버전 존재) |
| `archived` | 보존용 |

### 문서 유형별 Status 값

| 유형 | 허용 값 | 초기값 |
|------|---------|--------|
| adr | proposed, accepted, deprecated, superseded | proposed |
| rfc | draft, discussion, accepted, rejected, withdrawn | draft |
| incident | investigating, identified, monitoring, resolved | investigating |
| 기타 | draft, review, approved, active, deprecated, archived | draft |

### 문서 유형별 추가 필드

#### ADR (21-ADR)

```yaml
---
id: "21.01"
title: "Database Selection"
status: proposed               # proposed | accepted | deprecated | superseded
date: 2024-01-15
decision_makers: ["@alice", "@bob"]
supersedes: null               # 대체하는 문서 ID (optional)
superseded_by: null            # 이 문서를 대체하는 문서
---
```

#### RFC (25-RFC)

```yaml
---
id: "25.01"
title: "Microservices Migration"
status: draft                  # draft | discussion | accepted | rejected | withdrawn
date: 2024-01-15
author: "@alice"
decision_date: null            # 결정 예정일
---
```

#### Incident (63-Incidents)

```yaml
---
id: "63.01"
title: "Server Outage"
severity: SEV2                 # SEV1 | SEV2 | SEV3
status: investigating          # investigating | identified | monitoring | resolved
occurred_at: 2024-01-15T10:30:00Z
resolved_at: null
owner: "@oncall"
---
```

## 영역 구조

```
00-09  System       시스템/메타 (인덱스, 템플릿)
10-19  Overview     프로젝트 개요 (README, 로드맵)
20-29  Architecture 아키텍처/설계 (ADR, 시스템 설계)
30-39  API          API/인터페이스 (REST, GraphQL)
40-49  Development  개발 가이드 (컨벤션, 테스트)
50-59  Process      프로세스 (요구사항, 회의록)
60-69  Operations   운영 (배포, 인시던트)
70-79  Knowledge    지식베이스 (트러블슈팅, FAQ)
80-89  Reference    참조 (외부 문서, 용어집)
90-99  Archive      아카이브 (폐기 문서)
```

## 카테고리 상세

### 00-09 System

| Cat | 이름 | 용도 |
|-----|------|------|
| 00 | Index | JDex (전체 인덱스) |
| 01 | Templates | 문서 템플릿 |
| 02 | Guidelines | 작성 가이드라인 |

### 20-29 Architecture

| Cat | 이름 | 용도 |
|-----|------|------|
| 21 | ADR | Architecture Decision Records |
| 22 | System-Design | 시스템 설계 문서 |
| 23 | Data-Model | 데이터 모델, ERD |
| 24 | Diagrams | 아키텍처 다이어그램 |
| 25 | RFC | 기술 제안서 |

### 30-39 API

| Cat | 이름 | 용도 |
|-----|------|------|
| 31 | REST-API | REST API 명세 |
| 32 | GraphQL | GraphQL 스키마 |
| 33 | Events | 이벤트/메시지 스키마 |
| 34 | SDK | SDK 문서 |

### 40-49 Development

| Cat | 이름 | 용도 |
|-----|------|------|
| 41 | Setup | 환경 설정 가이드 |
| 42 | Conventions | 코드 컨벤션 |
| 43 | Contributing | 기여 가이드 |
| 44 | Testing | 테스트 가이드 |
| 45 | Security | 보안 가이드 |

### 50-59 Process

| Cat | 이름 | 용도 |
|-----|------|------|
| 51 | Requirements | 요구사항 명세 |
| 52 | Tasks | 작업 관리 |
| 53 | Meetings | 회의록 |
| 54 | Retrospectives | 회고 |

### 60-69 Operations

| Cat | 이름 | 용도 |
|-----|------|------|
| 61 | Deployment | 배포 가이드 |
| 62 | Monitoring | 모니터링 설정 |
| 63 | Incidents | 인시던트 기록 |
| 64 | Runbooks | 운영 절차서 |

### 70-79 Knowledge

| Cat | 이름 | 용도 |
|-----|------|------|
| 71 | Troubleshooting | 트러블슈팅 가이드 |
| 72 | Learnings | 학습 내용 |
| 73 | Best-Practices | 베스트 프랙티스 |
| 75 | FAQ | 자주 묻는 질문 |

## JDex 형식

```markdown
# JDex - 문서 인덱스

## 20-29 Architecture

### 21 ADR
- 21.01 Database Selection
- 21.02 API Versioning

### 22 System-Design
- 22.01 Authentication Flow

## 30-39 API

### 31 REST-API
- 31.01 User API
- 31.02 Order API
```

## 검증 규칙

1. **ID 유일성**: 카테고리 내 ID 중복 불가
2. **순차 할당**: 새 ID는 마지막 ID + 1
3. **JDex 동기화**: 파일 생성/삭제 시 JDex 업데이트 필수
4. **상태 전이**: draft → review → approved → active → deprecated
5. **링크 유효성**: 문서 간 참조 링크 검증

## CLI 도구

### 구조

```
bin/
├── jd              # 메인 디스패처
└── jd.d/           # 서브커맨드 폴더
    ├── new         # 문서 생성
    ├── index       # JDex 관리
    └── health      # 건강도 체크
```

### 명령어

| 명령 | 설명 |
|------|------|
| `jd new {type} "{title}"` | 문서 생성 |
| `jd index update` | JDex 동기화 |
| `jd index check` | 일관성 검사 |
| `jd index list` | 문서 목록 |
| `jd health` | 건강도 체크 |
| `jd health --fix` | 자동 수정 |

### 확장

새 서브커맨드 추가:

```bash
# ~/bin/jd.d/stats 생성
#!/bin/bash
set -euo pipefail
DOCS_DIR="${DOCS_DIR:-docs}"
# 문서 통계 출력
find "$DOCS_DIR" -name "*.md" | wc -l
```

```bash
chmod +x ~/bin/jd.d/stats
jd stats  # 즉시 사용 가능
```

**확장 규칙**:
- `jd.d/` 폴더에 실행 파일 추가
- 파일명 = 서브커맨드명
- `set -euo pipefail` 권장
- `DOCS_DIR` 환경변수 사용

### 환경변수

| 변수 | 기본값 | 설명 |
|------|--------|------|
| `DOCS_DIR` | `docs` | 문서 루트 디렉토리 |
| `JD_DIR` | `$(dirname $0)/jd.d` | 서브커맨드 디렉토리 |
| `STALE_DAYS` | `180` | 오래된 문서 기준일 |
