# Rule Creator Reference

Rules 상세 사례와 패턴 레퍼런스입니다.

## Glob 패턴 상세

### 기본 패턴

| 패턴 | 매칭 대상 | 설명 |
|------|----------|------|
| `**/*.ts` | 모든 TypeScript | 재귀적 매칭 |
| `*.md` | 루트 Markdown만 | 현재 디렉토리만 |
| `src/**/*` | src/ 하위 전체 | 디렉토리 포함 |
| `src/components/*.tsx` | 직접 자식만 | 중첩 제외 |

### 확장자 조합

| 패턴 | 매칭 대상 |
|------|----------|
| `**/*.{ts,tsx}` | TS와 TSX 모두 |
| `**/*.{js,jsx,ts,tsx}` | 모든 JS/TS |
| `**/*.{css,scss,less}` | 모든 스타일 |

### 디렉토리 조합

| 패턴 | 매칭 대상 |
|------|----------|
| `{src,lib}/**/*.ts` | src/와 lib/ 하위 |
| `{app,pages}/**/*.tsx` | Next.js 라우트 |
| `{tests,__tests__}/**/*` | 테스트 디렉토리들 |

### 복합 패턴

```yaml
# 여러 패턴 조합
paths: src/**/*.{ts,tsx}, lib/**/*.ts, tests/**/*.test.ts

# 보안 민감 영역
paths: src/auth/**/*.*, src/payments/**/*.*, src/crypto/**/*.*

# 프론트엔드 전체
paths: src/components/**/*.*, src/hooks/**/*.*, src/pages/**/*.*
```

---

## 활용 사례별 상세 템플릿

### API 개발 규칙 (상세)

```markdown
---
paths: src/api/**/*.ts, src/routes/**/*.ts
---

# API Development Rules

## 입력 검증
- 모든 엔드포인트에 Zod 스키마로 입력 검증
- 검증 실패 시 400 에러와 상세 메시지 반환

## 에러 처리
- 표준 에러 응답 포맷:
  ```json
  { "error": { "code": "string", "message": "string" } }
  ```
- HTTP 상태 코드 정확히 사용 (400, 401, 403, 404, 500)

## 로깅
- correlation ID로 요청 추적
- 민감 데이터 마스킹 (PII, credentials)

## 문서화
- OpenAPI/Swagger 주석 포함
- 요청/응답 예시 제공
```

### 테스트 작성 규칙 (상세)

```markdown
---
paths: **/*.test.ts, **/*.test.tsx, **/*.spec.ts
---

# Test Standards

## 네이밍
- 테스트명: "should [action] when [condition]"
- describe 블록: 테스트 대상 명시

## 구조
- Arrange-Act-Assert 패턴
- 테스트당 assertion 하나 권장
- fixtures는 __fixtures__/ 또는 conftest 사용

## Mock
- 외부 의존성만 Mock
- 구현 세부사항 Mock 금지
- Mock 남용 시 설계 재검토

## 커버리지
- 새 기능: 80% 이상
- 버그 수정: 회귀 테스트 필수
```

### 보안 규칙 (상세)

```markdown
---
paths: src/auth/**/*.*, src/payments/**/*.*, src/admin/**/*.*
---

# Security-Critical Rules

## 절대 금지
- 민감 데이터 로깅 (passwords, tokens, card numbers, SSN)
- 하드코딩된 credentials
- eval() 또는 동적 코드 실행

## 필수 사항
- 함수 경계에서 모든 입력 검증
- parameterized query 사용 (SQL injection 방지)
- HTTPS only 통신
- 적절한 CORS 설정

## 인증/인가
- JWT 만료 시간 설정
- refresh token rotation
- rate limiting 적용
```

### React 컴포넌트 규칙 (상세)

```markdown
---
paths: src/components/**/*.tsx, src/hooks/**/*.ts
---

# React Development Rules

## 컴포넌트
- 함수형 컴포넌트만 사용
- Props 타입 interface로 명시
- 단일 책임 원칙 준수

## Hooks
- 복잡한 로직은 custom hook으로 분리
- useEffect 의존성 배열 정확히 명시
- cleanup 함수 필수 (이벤트, 타이머, 구독)

## 상태 관리
- 로컬 상태: useState
- 컴포넌트 간 공유: Context 또는 상태 관리 라이브러리
- 서버 상태: React Query / SWR

## 성능
- 무거운 컴포넌트: React.memo 고려
- 비싼 계산: useMemo
- 콜백 안정화: useCallback
```

### 데이터베이스 규칙 (상세)

```markdown
---
paths: src/db/**/*.*, prisma/**/*.*, src/repositories/**/*.*
---

# Database Rules

## 쿼리
- ORM 사용 권장 (raw query 최소화)
- N+1 문제 방지 (eager loading)
- 인덱스 활용 확인

## 마이그레이션
- 롤백 방법 명시 필수
- 프로덕션 데이터 복사본에서 테스트
- 대용량 테이블 변경 시 무중단 전략

## 트랜잭션
- 여러 테이블 수정 시 트랜잭션 사용
- 적절한 isolation level 선택
- deadlock 방지 고려
```

---

## 전역 규칙 예시 (paths 없음)

### 코드 스타일

```markdown
# Code Style

- TypeScript strict mode 사용
- ESLint/Prettier 규칙 준수
- 2-space 들여쓰기
- 명확한 변수/함수 이름 사용
```

### Git 워크플로우

```markdown
# Git Workflow

- feature 브랜치에서 작업
- main에 직접 커밋 금지
- PR 전 lint/test 통과 필수
- 커밋 메시지: conventional commits
```

### 문서화

```markdown
# Documentation

- 공개 API: JSDoc/TSDoc 필수
- 복잡한 로직: 인라인 주석
- README: 설치, 실행, 배포 방법
```

---

## Symlink 활용

공유 규칙을 여러 프로젝트에서 재사용:

```bash
# 공유 디렉토리 symlink
ln -s ~/shared-claude-rules .claude/rules/shared

# 개별 파일 symlink
ln -s ~/company-standards/security.md .claude/rules/security.md
```
