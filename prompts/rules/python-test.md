---
description: pytest 문법 레퍼런스
paths:
  - "**/*_test.py"
  - "**/test_*.py"
---

pytest 도구/문법 레퍼런스입니다. 테스트 원칙은 `/test-guidelines` 참조.

## Fixture

```python
@pytest.fixture(scope="function")  # function(기본), class, module, session
def db_connection():
    conn = create_connection()
    yield conn  # teardown은 yield 이후
    conn.close()

@pytest.fixture(autouse=True)  # 테스트마다 자동 실행
def reset_state():
    State.clear()
```

## Parametrize

```python
@pytest.mark.parametrize("input,expected", [
    ("hello", 5),
    ("", 0),
    ("한글", 2),
])
def test_length(input, expected):
    assert len(input) == expected
```

## conftest.py

프로젝트/디렉토리별 공유 fixture 정의. import 없이 자동 적용.

## Mock

```python
# monkeypatch (pytest 내장)
def test_env(monkeypatch):
    monkeypatch.setenv("API_KEY", "test")
    monkeypatch.setattr(module, "func", lambda: 42)

# pytest-mock
def test_api(mocker):
    mocker.patch('requests.get', return_value=Mock(status_code=200))
```

## Assert & 예외

```python
assert result == {"key": "value"}
assert "substring" in text

with pytest.raises(ValueError, match=r"invalid.*format"):
    parse_data(invalid_input)
```
