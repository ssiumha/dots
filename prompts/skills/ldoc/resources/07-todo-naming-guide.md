# TODO Naming Guide

TODO 파일명(slug) 작성 규칙입니다.

## Slug 생성 원칙 (Self-Descriptive)

1. **동사 구체화**: fix → correct/add/refactor/resolve
2. **명사 구체화**: types → nullable-field-types, auth → jwt-auth
3. **길이 허용**: 4-6 단어 OK (명확성 > 간결성)

## 예시

| Before (나쁜 예) | After (좋은 예) |
|------------------|-----------------|
| admin-api-fix-openapi-types | admin-api-correct-openapi-nullable-field-types |
| update-user-service | add-email-verification-to-user-registration |
| fix-auth-bug | resolve-jwt-token-expiry-validation-error |
| implement-feature | implement-payment-refund-workflow |

## 자가 점검

파일명만 봐도 작업 내용 파악 가능한가?

- ❌ `fix-api.md` - 무엇을 고치는지 불명확
- ✅ `fix-user-api-null-response-error.md` - 명확함

## 복수 작업 분할 시

관련 TODO들은 공통 접두사 사용:

```
user-registration-validate-email.md
user-registration-send-welcome-email.md
user-registration-create-default-settings.md
```
