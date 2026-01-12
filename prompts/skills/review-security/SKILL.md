---
name: review-security
description: Reviews code for security vulnerabilities. Use when implementing auth, handling user input, adding APIs, or before deployment. Checks OWASP Top 10, credentials, injection.
---

# Security Review

코드 변경사항을 보안 관점에서 체크하는 전문 리뷰 스킬입니다.

## Instructions

### 리뷰 범위 결정

사용자 요청에 따라 워크플로우 선택:

1. **변경된 파일만 리뷰** (기본) - Git diff 기반
2. **특정 파일/디렉토리 리뷰** - 사용자 지정 경로
3. **전체 프로젝트 스캔** - 초기 설정, 정기 감사

### Workflow 1: 변경된 파일 보안 리뷰 (기본)

1. `git diff --name-only HEAD`로 변경 파일 확인
2. 코드 파일 필터링 (`.js`, `.ts`, `.py`, `.go` 등)
3. 각 파일 Read → 보안 체크리스트 적용
4. 심각도별 리포트 생성

### Workflow 2: 특정 파일/디렉토리 리뷰

1. Glob으로 대상 파일 확인
2. 각 파일 Read → 보안 체크리스트 적용
3. 리포트 생성

### Workflow 3: 전체 프로젝트 스캔

1. 우선순위 디렉토리 결정: `auth/`, `api/`, `config/`
2. 순차 리뷰, 심각한 이슈 즉시 보고
3. 종합 리포트

## 보안 체크리스트

상세 패턴은 resources/ 참조:

| 심각도 | 항목 | 참조 |
|--------|------|------|
| Critical | Hardcoded Credentials, SQL Injection, XSS, SSRF, Command Injection, Path Traversal | `01-critical-patterns.md` |
| High | CSRF, File Upload, Deserialization, Weak Crypto, JWT, Mass Assignment, Open Redirect | `02-high-patterns.md` |
| Medium | HTTPS, Error Exposure, Rate Limiting, Cookie Settings, Input Validation, XXE | `03-medium-low-patterns.md` |
| Low | CORS, Security Headers, Server Banner, Dependencies | `03-medium-low-patterns.md` |

### Secret 탐지

서비스별 API 키 패턴: `04-secret-patterns.md`

| 서비스 | 패턴 예시 |
|--------|-----------|
| AWS | `AKIA[0-9A-Z]{16}` |
| GitHub | `ghp_[A-Za-z0-9]{36}` |
| OpenAI | `sk-[A-Za-z0-9]{48}`, `sk-proj-[A-Za-z0-9-_]{48,}` |
| Anthropic | `sk-ant-[A-Za-z0-9-_]{95}` |
| Stripe | `sk_live_[0-9a-zA-Z]{24,}` |

### AI 생성 코드 주의점

`05-ai-code-security.md` 참조:
- 빈 catch 블록
- 입력 검증 누락
- 하드코딩된 설정값
- 오래된 API 사용

## 외부 도구 연동

`06-tool-integration.md` 참조:

```bash
# Semgrep (권장)
semgrep --config=p/owasp-top-ten .

# Secret scanning
gitleaks detect --source .

# Dependency check
npm audit  # Node.js
pip-audit  # Python
```

## 언어별 특화 패턴

`07-language-specific.md` 참조:

| 언어 | 주요 위험 |
|------|-----------|
| Python | pickle, eval, shell=True |
| JavaScript | XSS, prototype pollution |
| Go | text/template, SQL concat |
| Rust | unsafe blocks, unwrap |
| Java | deserialization, XXE |

## 리포트 형식

```markdown
# Security Review Report

**Date:** YYYY-MM-DD
**Files:** N files
**Issues:** X Critical, Y High, Z Medium, W Low

---

## Critical Issues

### 1. [Issue Title]
**File:** `path/file.ts:42`
**Description:** ...
**Code:** `...`
**Fix:** ...
**Reference:** [OWASP Link]

---

## Summary
- Good practices: [list]
- Areas needing attention: [list]
```

## 중요 원칙

1. **False Positive 최소화**: 확실한 이슈만 보고
2. **Context 고려**: 테스트 코드는 완화된 기준
3. **구체적 제안**: "어떻게 고치는지" 제시
4. **OWASP 참조**: 공식 문서 링크 제공
5. **긍정적 피드백**: 잘한 부분도 언급

## OWASP Top 10 (2021)

1. Broken Access Control
2. Cryptographic Failures
3. Injection
4. Insecure Design
5. Security Misconfiguration
6. Vulnerable Components
7. Authentication Failures
8. Data Integrity Failures
9. Logging Failures
10. **Server-Side Request Forgery (SSRF)**
