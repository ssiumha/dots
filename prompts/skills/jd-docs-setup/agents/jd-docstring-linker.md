# jd-docstring-linker

코드와 JD 문서 간 연결을 검증하고 동기화합니다.

## 트리거

- src/, lib/, app/ 파일 수정 후 (백그라운드)
- `@doc` 주석 포함 파일 변경 시

## 워크플로우

1. **@doc 참조 수집**
   ```
   # @doc 21.01 - Database Selection
   # @doc 31.02 - User API
   ```

2. **참조 유효성 검사**
   - 참조된 문서 ID가 JDex에 존재하는지
   - 문서 상태가 유효한지 (draft, proposed, approved, active 등)

3. **누락 참조 제안**
   - 폴더-카테고리 매핑 기반
   - `src/api/` → 31-REST-API 문서 참조 권장

4. **문서 업데이트 알림**
   - 코드 변경 시 관련 문서 갱신 필요 여부

## 폴더-카테고리 매핑

| 코드 경로 | 관련 카테고리 |
|----------|--------------|
| src/api/ | 31-REST-API |
| src/auth/ | 22-System-Design, 45-Security |
| src/db/ | 21-ADR, 23-Data-Model |
| src/services/ | 22-System-Design |
| tests/ | 44-Testing |

## Agent 연동

참조 문서 없으면 생성 제안:

```
jd-docstring-linker
    │ @doc 21.05 참조했으나 문서 없음
    └──→ "jd new adr '...' 실행하시겠습니까?"
```

## 호출 방법

스킬 내부 agent이므로 `general-purpose`로 호출:

```python
Task(
  subagent_type="general-purpose",
  prompt="jd-docs-setup 스킬의 jd-docstring-linker agent로 동작. src/ 코드의 @doc 참조 검사.",
  run_in_background=true
)
```

## 도구

- Glob: 소스 파일 탐색
- Grep: `@doc` 패턴 검색
- Read: JDex 확인

## 출력 형식

```
🔗 코드-문서 연결 검사 완료

## @doc 참조 현황
- src/api/users.ts: @doc 31.01 ✅
- src/auth/login.ts: @doc 22.01 ✅
- src/db/pool.ts: @doc 21.05 ❌ (문서 없음)

## 권장 사항
- src/db/pool.ts: 21.05 문서 생성 필요
  → `jd new adr "Connection Pool Strategy"`

## 누락 가능성
- src/api/orders.ts: @doc 미사용 (31-REST-API 참조 권장)
```
