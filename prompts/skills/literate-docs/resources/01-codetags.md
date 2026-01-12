# 코드 태그 (Codetags)

PEP 350 기반의 범용 코드 태그 컨벤션입니다.

## 표준 태그

| 태그 | 의미 | 심각도 |
|------|------|--------|
| `TODO` | 추후 구현할 기능/작업 | 낮음 |
| `FIXME` | 버그, 반드시 수정 필요 | 높음 |
| `HACK` | 임시 해결책, 기술 부채 | 중간 |
| `XXX` | 주의 필요, 논의 필요 | 높음 |
| `NOTE` | 참고 사항, 설명 | 정보 |
| `BUG` | 알려진 버그 | 높음 |
| `OPTIMIZE` | 성능 개선 필요 | 중간 |
| `REVIEW` | 코드 리뷰 필요 | 중간 |

## 확장 태그 (Literate Docs)

| 태그 | 의미 | 용도 |
|------|------|------|
| `WHY` | 결정 이유 | 왜 이렇게 구현했는지 |
| `DECISION` | 아키텍처 결정 | 대안과 선택 이유 |
| `HISTORY` | 변경 이력 | 언제 무엇이 바뀌었는지 |
| `DEPRECATED` | 폐기 예정 | 대체 방법 안내 |
| `SEE` | 참조 | 관련 코드/문서 링크 |

## 포맷 규칙

### 기본 형식

```
# TAG: 설명
// TAG: 설명
/* TAG: 설명 */
/// TAG: 설명
```

### 티켓 연동

```python
# TODO(#123): 리팩토링 필요
# FIXME(JIRA-456): 메모리 누수 해결
```

### 날짜/담당자

```javascript
// TODO(2025-02-01): 다음 릴리즈에 구현
// FIXME(@username): 긴급 수정 필요
// HACK(2025-01-08, @alice): 임시 우회, #789 해결 후 제거
```

### 만료일 (권장)

```go
// TODO(expires:2025-03-01): v2 API로 마이그레이션
// DEPRECATED(until:2025-06-01): UseNewMethod() 사용 권장
```

## 멀티라인 형식

### 간단한 블록

```python
# WHY: 성능 이슈로 dict 대신 list 사용
#   - dict 조회: O(1)이지만 메모리 2배
#   - list 순회: O(n)이지만 캐시 효율적
#   - 데이터 크기 100 이하에서 list가 빠름
```

### DECISION 블록

```python
# DECISION: ORM 대신 raw SQL 사용
#   Context: 복잡한 집계 쿼리 성능 이슈
#   Options:
#     1. Django ORM - 느림 (3초)
#     2. Raw SQL - 빠름 (0.1초)
#     3. 캐싱 - 복잡성 증가
#   Choice: Raw SQL
#   Consequence: SQL 직접 관리 필요
```

### HISTORY 블록

```python
# HISTORY:
#   2025-01-08: 초기 구현 (v1.0)
#   2025-01-15: 성능 최적화 - O(n²) → O(n log n)
#   2025-01-20: 에러 핸들링 추가 (#234)
```

## 언어별 문법

### Python

```python
# TODO: 단일 라인
# NOTE: 참고 사항

"""
WHY: Docstring 내에서도 사용 가능
  - 이유 1
  - 이유 2
"""
```

### JavaScript/TypeScript

```javascript
// TODO: 단일 라인
/* FIXME: 블록 주석 */

/**
 * @todo 구현 필요
 * @see https://example.com
 *
 * WHY: JSDoc 내에서도 사용
 */
```

### Go

```go
// TODO: 단일 라인만 사용
// NOTE: Go는 블록 주석 비권장

// WHY: 멀티라인은 연속 주석으로
//   - 이유 1
//   - 이유 2
```

### Rust

```rust
// TODO: 단일 라인
// FIXME: 수정 필요

/// WHY: rustdoc 주석에서도 사용
///   - 이유 설명
```

## 도구 지원

### IDE 하이라이팅

대부분의 IDE가 TODO, FIXME, HACK을 자동 하이라이팅:
- VS Code: Todo Tree 확장
- JetBrains: 내장 TODO 뷰
- Vim: todo.txt 플러그인

### 검색

```bash
# 모든 TODO 찾기
grep -rn "TODO" --include="*.py"

# 만료된 TODO 찾기 (날짜 기반)
grep -rn "TODO.*2024" --include="*.py"
```

### Linting

```javascript
// ESLint: no-warning-comments
// 티켓 없는 TODO 금지
"no-warning-comments": ["error", {
  "terms": ["todo", "fixme"],
  "location": "start"
}]
```

## Best Practices

1. **티켓 연동**: TODO/FIXME에는 항상 티켓 번호
2. **만료일**: 임시 코드에는 만료일 명시
3. **담당자**: 큰 팀에서는 담당자 지정
4. **정기 정리**: 주기적으로 오래된 태그 정리
5. **CI 연동**: 만료된 TODO를 빌드 실패로 처리

## Anti-patterns

| ❌ 문제 | ✅ 해결 |
|--------|--------|
| `// TODO: fix this` | `// TODO(#123): null 체크 추가` |
| `// HACK` (설명 없음) | `// HACK: API 버그 우회, v2에서 제거` |
| 5년 된 TODO | 티켓 생성 또는 삭제 |
| FIXME 방치 | 즉시 수정 또는 티켓화 |
