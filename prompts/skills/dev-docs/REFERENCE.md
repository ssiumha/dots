# Dev Docs Reference

## Frontmatter 스키마

### plan.md

```yaml
---
task: string           # 작업명 (kebab-case)
created: YYYY-MM-DD    # 생성일
updated: YYYY-MM-DD    # 마지막 수정일
status: string         # in-progress | blocked | completed
completed: YYYY-MM-DD  # 완료일 (status가 completed일 때)
---
```

**status 값:**
- `in-progress`: 현재 진행 중
- `blocked`: 블로커로 인해 멈춤
- `completed`: 완료됨

### context.md

```yaml
---
task: string           # 작업명 (plan.md와 동일)
updated: YYYY-MM-DD    # 마지막 수정일
---
```

### tasks.md

```yaml
---
task: string           # 작업명 (plan.md와 동일)
updated: YYYY-MM-DD    # 마지막 수정일
---
```

## 디렉토리 구조

```
~/docs/dev/{project}/
├── active/                    # 진행 중인 작업들
│   ├── feature-user-auth/
│   │   ├── plan.md           # 승인된 계획
│   │   ├── context.md        # 핵심 컨텍스트
│   │   └── tasks.md          # 체크리스트
│   └── bugfix-login-timeout/
│       ├── plan.md
│       ├── context.md
│       └── tasks.md
└── archive/                   # 완료된 작업들
    └── feature-payments/
        ├── plan.md
        ├── context.md
        └── tasks.md
```

## 문서 섹션 구조

### plan.md

```markdown
# {작업명}

## 목표
[작업의 목표와 범위]

## 단계
1. [단계 1]
2. [단계 2]
...

## 리스크
- [잠재적 리스크 1]
- [리스크 2]

## 성공 기준
- [기준 1]
- [기준 2]

## 변경 이력 (선택)

### YYYY-MM-DD: [변경 제목]
**변경 내용:**
- [무엇이 변경되었는지]

**이유:**
- [왜 변경했는지]

---
```

### context.md

```markdown
# Context: {작업명}

## 핵심 파일
- `/path/to/file` - [역할]

## 아키텍처 노트
[아키텍처 결정사항]

## 의사결정

### YYYY-MM-DD: [결정 제목]
**결정 내용:**
[내용]

**이유:**
[이유]

**대안:**
[고려했던 선택지]

**영향:**
[영향 범위]

---

## 관련 문서
- Living Docs: [[doc-id]] - [설명]
- External: [URL] - [설명]

## 주의사항
- [주의점]

## 메모
[자유로운 메모]
```

### tasks.md

```markdown
# Tasks: {작업명}

## 진행 상황

### Phase 1: [단계명]
- [ ] Task 1
- [ ] Task 2

### Phase 2: [단계명]
- [ ] Task 3

## 완료된 작업
- [x] ~~Task~~ (YYYY-MM-DD)

## 블로커
[진행을 막는 이슈]

## 다음 단계
1. [우선순위 1]
2. [우선순위 2]

## 통계
- 전체: N
- 완료: M
- 진행률: P%
```

## Living Docs 통합 매핑

Dev Docs 완료 시, 다음과 같이 Living Docs로 통합:

| Dev Docs | Living Docs | 조건 |
|----------|-------------|------|
| context.md > 의사결정 | `~/docs/{project}/decisions/{slug}.md` | 중요한 결정사항 |
| context.md > 아키텍처 노트 | `~/docs/{project}/knowledge/architecture/{topic}.md` | 재사용 가능한 지식 |
| context.md > 주의사항 | `~/docs/{project}/knowledge/{category}/{topic}.md` | 팀이 알아야 할 함정 |
| tasks.md > 완료된 작업 | Living Docs 문서의 히스토리 섹션 | 중요한 마일스톤 |

## 작업명 네이밍 규칙

**형식**: `{type}-{scope}-{what}-{how/tech}`

디렉토리명만 봐도 무엇을 어떻게 하는 작업인지 즉시 파악할 수 있어야 합니다.

### Type 접두사

- `feature-`: 새 기능 추가
- `bugfix-`: 버그 수정
- `refactor-`: 리팩토링
- `perf-`: 성능 개선
- `docs-`: 문서 작업
- `test-`: 테스트 추가/수정
- `chore-`: 기타 작업

### 디테일한 네이밍 원칙

**1. 핵심 키워드 포함**
제목에 기술 스택, 범위, 목적이 명확히 드러나야 합니다.

❌ **나쁜 예** (애매함):
- `feature-auth`
- `bugfix-login`
- `refactor-api`

✅ **좋은 예** (구체적):
- `feature-session-based-user-auth`
- `bugfix-login-timeout-5xx-error`
- `refactor-rest-api-to-graphql`

**2. Scope 명시**
어느 범위/모듈에 영향을 주는지 명시합니다.

- `feature-user-profile-avatar-upload`
- `bugfix-payment-stripe-webhook-retry`
- `refactor-admin-dashboard-table-component`

**3. 기술 스택 포함** (해당 시)
특정 기술을 사용한다면 이름에 포함합니다.

- `feature-redis-session-store`
- `perf-postgresql-index-optimization`
- `refactor-axios-to-fetch-api`

**4. 문제/해결 명시** (bugfix)
무엇이 문제고 어떻게 해결하는지 드러냅니다.

❌ `bugfix-login-error`
✅ `bugfix-login-csrf-token-expiration`

❌ `bugfix-payment-failed`
✅ `bugfix-payment-stripe-idempotency-key-reuse`

**5. Before/After 명시** (refactor)
무엇에서 무엇으로 바뀌는지 드러냅니다.

- `refactor-class-components-to-hooks`
- `refactor-rest-api-to-graphql`
- `refactor-redux-to-zustand`

### 네이밍 패턴 예시

**Feature (기능 추가):**
```
feature-{module}-{feature}-{tech}
```
- `feature-auth-jwt-token-refresh`
- `feature-payment-stripe-subscription-billing`
- `feature-chat-websocket-real-time-messaging`
- `feature-file-s3-multipart-upload`
- `feature-search-elasticsearch-full-text-search`

**Bugfix (버그 수정):**
```
bugfix-{module}-{problem}-{root-cause}
```
- `bugfix-login-csrf-token-expiration`
- `bugfix-checkout-price-calculation-rounding-error`
- `bugfix-upload-image-memory-leak-on-resize`
- `bugfix-api-rate-limit-429-on-burst-traffic`

**Refactor (리팩토링):**
```
refactor-{module}-{from}-to-{to}
```
- `refactor-auth-class-to-hooks`
- `refactor-api-rest-to-graphql`
- `refactor-state-redux-to-zustand`
- `refactor-db-mysql-to-postgresql`

**Performance (성능 개선):**
```
perf-{module}-{optimization}-{method}
```
- `perf-dashboard-lazy-loading-code-splitting`
- `perf-images-webp-conversion-cdn-caching`
- `perf-db-query-index-optimization`
- `perf-api-response-compression-gzip`

### 네이밍 체크리스트

작업명을 정할 때 다음을 확인하세요:

- [ ] 디렉토리명만 봐도 무슨 작업인지 파악되는가?
- [ ] 핵심 기술 스택이나 방법이 포함되었는가?
- [ ] 범위(scope)가 명확한가?
- [ ] 나중에 검색할 때 찾기 쉬운 키워드가 있는가?
- [ ] kebab-case 형식인가?
- [ ] 50자 이하인가? (너무 길면 축약)

### 규칙 요약

- kebab-case 사용
- 소문자만 사용
- 구체적이고 디테일하게
- 기술 스택, 문제, 해결 방법 포함
- **50자 권장** (최대 64자)
- grep 없이 `ls`만으로도 내용 파악 가능하게

## 타임스탬프 규칙

- 형식: `YYYY-MM-DD` (ISO 8601)
- 시간대: 로컬 타임존
- 갱신 시점:
  - 문서 생성 시: `created`, `updated` 설정
  - 문서 읽기만: 갱신 안 함
  - 문서 수정 시: `updated` 갱신
  - 작업 완료 시: `status: completed`, `completed` 설정

## Living Docs 통합 체크리스트

작업 완료 시 다음을 확인:

### 의사결정 (context.md > 의사결정)
- [ ] 이 결정이 다른 작업/팀에게 영향을 주는가?
- [ ] 이 결정을 나중에 참조할 필요가 있는가?
- [ ] 왜 이렇게 결정했는지가 명확한가?

→ **Yes**: `~/docs/{project}/decisions/{slug}.md` 생성

### 아키텍처 지식 (context.md > 아키텍처 노트)
- [ ] 이 아키텍처가 다른 기능에도 적용되는가?
- [ ] 팀원들이 이 패턴을 따라야 하는가?
- [ ] 시스템 구조에 영향을 주는가?

→ **Yes**: `~/docs/{project}/knowledge/architecture/{topic}.md` 생성 또는 업데이트

### 주의사항 (context.md > 주의사항)
- [ ] 다른 사람도 이 함정에 빠질 수 있는가?
- [ ] 문서화되지 않으면 시간을 낭비할 수 있는가?
- [ ] 보안이나 성능에 영향을 주는가?

→ **Yes**: 관련 `~/docs/{project}/knowledge/{category}/{topic}.md`에 추가

### 완료 기록 (tasks.md > 완료된 작업)
- [ ] 중요한 마일스톤인가?
- [ ] 나중에 변경 히스토리를 추적해야 하는가?

→ **Yes**: Living Docs 문서의 히스토리 섹션에 추가

## 예시

### 통합 전: dev-docs (작업 완료)

**context.md:**
```markdown
## 의사결정

### 2024-02-15: Session 기반 인증 선택

**결정 내용:**
JWT 대신 Session 기반 인증을 사용

**이유:**
- 서버 측 제어 필요 (즉시 세션 무효화)
- CSRF 보호가 더 간단
- 토큰 갱신 로직 불필요

**대안:**
- JWT: 상태 비저장이지만 즉시 무효화 어려움
- OAuth 2.0: 오버스펙

**영향:**
- Redis 세션 스토어 필요
- 서버 메모리 사용 증가
```

### 통합 후: ldoc

**~/docs/myproject/decisions/auth-session-based.md:**
```markdown
---
id: dec-auth-session
decided: 2024-02-15
impacts: [know-arch-auth, know-security-session]
status: accepted
---

# Session 기반 인증 선택

## 결정 내용

JWT 대신 Session 기반 인증을 사용하기로 결정했습니다.

## 컨텍스트

사용자 인증 시스템 구현 중, 인증 방식을 선택해야 했습니다.

## 고려한 대안

1. **JWT (JSON Web Token)**
   - 장점: Stateless, 서버 부하 적음
   - 단점: 즉시 무효화 어려움, 토큰 갱신 복잡

2. **OAuth 2.0**
   - 장점: 표준 프로토콜
   - 단점: 우리 요구사항에 오버스펙

3. **Session 기반** (선택)
   - 장점: 서버 측 제어, 즉시 무효화, CSRF 보호 간단
   - 단점: 서버 메모리 사용, Redis 필요

## 결정 이유

- **즉시 세션 무효화 필요**: 관리자가 사용자 세션을 즉시 종료할 수 있어야 함
- **보안 우선**: CSRF 보호가 더 간단
- **단순성**: 토큰 갱신 로직 불필요

## 영향

- Redis 세션 스토어 도입 필요
- 서버 메모리 사용 증가
- 관련 문서:
  - [[know-arch-auth]]: 인증 아키텍처
  - [[know-security-session]]: 세션 보안 정책

## 참조

- Dev Docs: feature-user-auth (2024-02-15 완료)
```

## 통합 프로세스

1. **작업 완료 확인**
   - tasks.md의 모든 체크박스 완료
   - 블로커 해결

2. **통합 대상 식별**
   - context.md 리뷰
   - 체크리스트 적용

3. **ldoc 스킬 활성화**
   - 의사결정 → decisions/
   - 아키텍처 → knowledge/architecture/
   - 주의사항 → knowledge/{category}/

4. **아카이브**
   - dev/active/ → dev/archive/ 이동
   - 통합 완료 기록

5. **역참조 추가**
   - Living Docs에 Dev Docs 작업 링크
   - "Dev Docs: {task-name} (YYYY-MM-DD 완료)" 메모
