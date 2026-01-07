---
paths:
  - src/**
  - lib/**
  - app/**
---

# 코드-문서 연결 규칙

코드 작성/수정 시 관련 JD 문서를 참조하고 연결합니다.

## 문서 참조 주석

코드에 관련 문서 참조 추가:

```python
# @doc 21.01 - Database Selection (ADR)
# @doc 31.02 - User API Specification
class UserRepository:
    ...
```

```typescript
/**
 * @doc 22.01 - Authentication Flow
 * @doc 41.01 - Security Guidelines
 */
export function authenticate() { ... }
```

## 폴더-카테고리 매핑

| 코드 경로 | 관련 문서 카테고리 |
|----------|-------------------|
| `src/api/` | 31-REST-API |
| `src/auth/` | 22-System-Design, 45-Security |
| `src/db/` | 21-ADR, 23-Data-Model |
| `src/services/` | 22-System-Design |
| `tests/` | 44-Testing |

## 워크플로우

1. **코드 작성 전**: 관련 JD 문서 확인
   ```
   jd index list    # 전체 목록
   ```

2. **코드 작성 시**: `@doc` 주석으로 참조
   ```
   # @doc 21.03 - Caching Strategy
   ```

3. **문서 업데이트 필요 시**: 코드 변경과 함께 문서도 갱신
   ```
   jd new adr "API Pagination Change"
   ```

## 자동 연결 (rules 파일)

특정 폴더에서 자동으로 관련 문서를 컨텍스트에 포함:

```markdown
<!-- .claude/rules/api-docs.md -->
---
paths:
  - src/api/**
---

API 코드 수정 시 아래 문서를 참조하세요:
- docs/30-39-API/31-REST-API/*.md
```
