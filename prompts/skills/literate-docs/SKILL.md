---
name: literate-docs
description: Adds docstrings, comments, and WHY/DECISION tags to code. Use when documenting functions/classes with language-specific standards (JSDoc, PEP 257, GoDoc).
---

# Literate Docs

코드와 문서를 통합하여 "왜"를 기록하는 문서화 패턴을 제공합니다.

**핵심 철학** (Knuth's Literate Programming):
- 코드에 문서를 넣는 게 아니라, 문서에 코드를 넣는다
- **왜** 이렇게 했는지 설명 (what이 아닌 why)
- 대안과 거부 이유 기록
- 사람이 읽기 좋은 순서로 구성

## Instructions

### 워크플로우 1: 언어 감지 → 표준 적용

파일 확장자로 언어 감지 후 해당 표준 적용:

| 확장자 | 언어 | 표준 | 리소스 |
|--------|------|------|--------|
| `.py` | Python | PEP 257 Docstrings | `02-python.md` |
| `.js`, `.ts`, `.jsx`, `.tsx` | JavaScript/TypeScript | JSDoc/TSDoc | `03-javascript.md` |
| `.go` | Go | GoDoc | `04-go.md` |
| `.rs` | Rust | rustdoc | `05-rust.md` |
| `.java`, `.kt`, `.kts` | Java/Kotlin | Javadoc/KDoc | `06-java.md` |
| `.md` | Markdown | GitHub Alerts + Frontmatter | `07-markdown.md` |

### 워크플로우 2: 코드 태그 삽입

범용 코드 태그 (`resources/01-codetags.md`):

| 태그 | 용도 | 예시 |
|------|------|------|
| `TODO` | 추후 작업 | `# TODO(#123): 리팩토링` |
| `FIXME` | 버그/수정필요 | `// FIXME: 경계 조건 처리` |
| `HACK` | 임시 해결책 | `/* HACK: 우회 처리 */` |
| `NOTE` | 참고 사항 | `/// NOTE: 순서 중요` |
| `WHY` | 결정 이유 | `# WHY: 성능 이슈로 선택` |

**Best Practice**: 티켓 번호 또는 날짜 포함
```
# TODO(2025-02-01): 다음 릴리즈에 구현
// FIXME(#456): 메모리 누수 해결
```

### 워크플로우 3: 히스토리/결정 기록

**Markdown (prompts, skills)**:
```yaml
---
name: skill-name
history:
  - 2025-01-08: 초기 생성
  - 2025-01-15: 버전 호환성 추가
decisions:
  - "도구 실행 기반: 명령어 생성이 주 목적"
---
```

**코드 파일**:
```python
# HISTORY:
#   2025-01-08: 초기 구현
#   2025-01-15: 성능 최적화 (O(n²) → O(n log n))
#
# DECISION: dict 대신 dataclass 사용
#   - 이유: 타입 힌트와 불변성 보장
#   - 대안: NamedTuple (메서드 추가 어려움)
```

## Quick Reference

### Python (PEP 257)

```python
def fetch_data(url: str, timeout: int = 30) -> dict:
    """URL에서 데이터를 가져온다.

    Args:
        url: 요청할 URL
        timeout: 타임아웃 초 (기본 30)

    Returns:
        JSON 응답을 dict로 반환

    Raises:
        TimeoutError: 타임아웃 초과 시

    Note:
        WHY: requests 대신 httpx 사용
        - 이유: async 지원 필요
    """
```

### JavaScript (JSDoc)

```javascript
/**
 * 사용자 정보를 가져온다.
 *
 * @param {string} userId - 사용자 ID
 * @returns {Promise<User>} 사용자 객체
 * @throws {NotFoundError} 사용자 없을 때
 *
 * @example
 * const user = await getUser('123');
 *
 * NOTE: 캐시 우선 조회 후 API 호출
 * WHY: API 호출 비용 절감
 */
```

### Go (GoDoc)

```go
// FetchData fetches data from the given URL.
//
// It returns the response body as bytes. If the request fails,
// it returns an error describing what went wrong.
//
// NOTE: Uses exponential backoff for retries.
// WHY: Network instability in production environment.
func FetchData(url string) ([]byte, error) {
```

### Markdown (GitHub Alerts)

```markdown
> [!NOTE]
> 이 설정은 0.30+ 버전에서만 지원됩니다.

> [!WARNING]
> 이 옵션은 deprecated 예정입니다.

> [!TIP]
> `fd` 사용 시 성능이 10배 향상됩니다.
```

## 중요 원칙

1. **Why over What**: 코드가 "무엇"을 하는지보다 "왜" 그렇게 했는지 설명
2. **대안 기록**: 선택하지 않은 방법과 그 이유도 함께 기록
3. **티켓 연동**: TODO/FIXME에는 이슈 번호 포함 (`TODO(#123)`)
4. **언어 표준 준수**: 각 언어의 공식 문서화 규격 따르기
5. **점진적 적용**: 새 코드부터 적용, 기존 코드는 수정 시 추가

## Examples

### 결정 문서화
```
User: "왜 이렇게 구현했는지 기록해줘"
→ 워크플로우 2 (코드 태그)
→ WHY/DECISION 태그 + 대안과 선택 이유 작성
```

### 히스토리 추가
```
User: "변경 이력 남겨줘"
→ 워크플로우 3 (히스토리/결정)
→ HISTORY 블록 또는 frontmatter history 필드
```

### 언어별 문서화
```
User: "이 Python 함수에 docstring 추가해줘"
→ 워크플로우 1 (언어 감지)
→ .py 확인 → 02-python.md 참조 → PEP 257 형식 적용
```

## Technical Details

- `REFERENCE.md`: Literate Programming 철학, ADR 연동
- `resources/01-codetags.md`: 범용 코드 태그 (PEP 350 기반)
- `resources/02-python.md`: PEP 257 Docstrings
- `resources/03-javascript.md`: JSDoc/TSDoc
- `resources/04-go.md`: GoDoc
- `resources/05-rust.md`: rustdoc
- `resources/06-java.md`: Javadoc
- `resources/07-markdown.md`: GitHub Alerts, YAML frontmatter
