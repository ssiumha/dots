# Python Docstrings (PEP 257)

Python 공식 문서화 표준입니다.

## 기본 규칙

- `"""트리플 쿼트"""` 사용 (단일 쿼트 아님)
- 백슬래시 포함 시 `r"""raw 문자열"""`
- 선언 바로 다음 줄에 위치
- 첫 줄은 요약 (마침표로 끝남)

## One-liner

```python
def square(n):
    """n의 제곱을 반환한다."""
    return n * n
```

**규칙**:
- 한 줄에 완결
- 명령형으로 작성 ("반환한다", "계산한다")
- 시그니처 반복 금지

## Multi-line

```python
def fetch_data(url: str, timeout: int = 30) -> dict:
    """URL에서 데이터를 가져온다.

    네트워크 요청을 수행하고 JSON 응답을 파싱한다.
    실패 시 재시도 로직이 포함되어 있다.

    Args:
        url: 요청할 URL (http/https)
        timeout: 타임아웃 초 (기본 30)

    Returns:
        JSON 응답을 dict로 반환
        예: {"status": "ok", "data": [...]}

    Raises:
        ValueError: URL이 유효하지 않을 때
        TimeoutError: 타임아웃 초과 시
        ConnectionError: 네트워크 오류 시

    Examples:
        >>> fetch_data("https://api.example.com/users")
        {"status": "ok", "data": [...]}

    Note:
        WHY: requests 대신 httpx 사용
        - 이유: async 지원, HTTP/2 지원
        - 대안: aiohttp (복잡한 API)
    """
```

## 스타일 가이드

### Google Style (권장)

```python
def function(arg1: str, arg2: int) -> bool:
    """한 줄 요약.

    상세 설명 (선택).

    Args:
        arg1: 첫 번째 인자 설명
        arg2: 두 번째 인자 설명

    Returns:
        반환값 설명

    Raises:
        ValueError: 예외 발생 조건
    """
```

### NumPy Style

```python
def function(arg1, arg2):
    """한 줄 요약.

    상세 설명.

    Parameters
    ----------
    arg1 : str
        첫 번째 인자 설명
    arg2 : int
        두 번째 인자 설명

    Returns
    -------
    bool
        반환값 설명

    Raises
    ------
    ValueError
        예외 발생 조건
    """
```

## 컨텍스트별 문서화

### 모듈

```python
"""requests 라이브러리 래퍼.

HTTP 요청을 단순화하고 에러 핸들링을 표준화한다.

HISTORY:
    2025-01-08: 초기 구현
    2025-01-15: 재시도 로직 추가

Typical usage:
    from mylib import http
    response = http.get("https://api.example.com")
"""
```

### 클래스

```python
class User:
    """사용자를 나타내는 클래스.

    Attributes:
        id: 고유 식별자
        name: 사용자 이름
        email: 이메일 주소

    DECISION: dataclass 대신 일반 클래스 사용
        - 이유: 커스텀 __init__ 로직 필요
        - 대안: attrs (외부 의존성)
    """

    def __init__(self, id: int, name: str, email: str):
        """User 인스턴스를 생성한다.

        Args:
            id: 고유 식별자 (양수)
            name: 사용자 이름 (1-100자)
            email: 유효한 이메일 주소
        """
```

### 상수/변수

```python
#: 기본 타임아웃 (초)
#: WHY: 30초는 대부분의 API 응답 시간 커버
DEFAULT_TIMEOUT = 30

MAX_RETRIES = 3  #: 최대 재시도 횟수
```

## Type Hints와 조합

```python
from typing import Optional, List

def search(
    query: str,
    limit: int = 10,
    filters: Optional[List[str]] = None
) -> List[dict]:
    """검색을 수행한다.

    타입 힌트가 있으므로 Args에서 타입 생략 가능.

    Args:
        query: 검색어
        limit: 결과 개수 제한
        filters: 필터 목록 (선택)

    Returns:
        검색 결과 목록
    """
```

## 도구 지원

### Sphinx

```python
# conf.py
extensions = ['sphinx.ext.autodoc', 'sphinx.ext.napoleon']
```

### pdoc

```bash
pdoc --html mymodule
```

### VS Code

`autoDocstring` 확장으로 자동 생성

## Codetags 통합

```python
def process(data):
    """데이터를 처리한다.

    TODO(#123): 배치 처리 지원 추가
    FIXME: 대용량 데이터에서 메모리 이슈

    WHY: pandas 대신 polars 사용
        - 이유: 메모리 효율 2배
        - 벤치마크: benchmarks/process_test.py
    """
```
