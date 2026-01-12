# 커스텀 도구

## @tool 데코레이터

```python
from claude_agent_sdk import tool

@tool(
    name="tool_name",           # 도구 이름 (snake_case)
    description="도구 설명",     # Claude가 보는 설명
    parameters={                # 파라미터 타입
        "param1": str,
        "param2": int,
        "optional_param": str | None,
    },
)
async def my_tool(args: dict) -> dict:
    """도구 구현"""
    param1 = args.get("param1")
    param2 = args.get("param2", 0)

    # 비즈니스 로직...
    result = do_something(param1, param2)

    # 반환 형식
    return {
        "content": [
            {"type": "text", "text": f"결과: {result}"}
        ]
    }
```

## MCP 서버로 래핑

```python
from claude_agent_sdk import tool, create_sdk_mcp_server, ClaudeAgentOptions

# 도구 정의
@tool("search", "검색", {"query": str})
async def search(args):
    return {"content": [{"type": "text", "text": f"결과: {args['query']}"}]}

@tool("notify", "알림", {"message": str})
async def notify(args):
    return {"content": [{"type": "text", "text": "전송 완료"}]}

# MCP 서버 생성
server = create_sdk_mcp_server(
    name="my-tools",
    version="1.0.0",
    tools=[search, notify],
)

# 에이전트에 연결
options = ClaudeAgentOptions(
    custom_tools=[server],
)
```

## 실전 예시

### 데이터베이스 도구

```python
import aiosqlite
import re

@tool(
    name="query_database",
    description="SQLite 데이터베이스 쿼리 실행 (SELECT만 허용)",
    parameters={"table": str, "columns": list, "where": str | None, "limit": int},
)
async def query_database(args: dict) -> dict:
    """파라미터화된 쿼리로 SQL 인젝션 방지"""
    table = args.get("table", "")
    columns = args.get("columns", ["*"])
    where = args.get("where")  # 예: "id = ?" (파라미터화)
    limit = min(args.get("limit", 100), 1000)
    db_path = "data.db"

    # 테이블/컬럼명 검증 (알파벳, 숫자, 언더스코어만)
    if not re.match(r'^[a-zA-Z_][a-zA-Z0-9_]*$', table):
        return {"content": [{"type": "text", "text": "오류: 유효하지 않은 테이블명"}]}

    for col in columns:
        if col != "*" and not re.match(r'^[a-zA-Z_][a-zA-Z0-9_]*$', col):
            return {"content": [{"type": "text", "text": f"오류: 유효하지 않은 컬럼명: {col}"}]}

    try:
        cols = ", ".join(columns)
        sql = f"SELECT {cols} FROM {table}"
        if where:
            sql += f" WHERE {where}"
        sql += f" LIMIT {limit}"

        async with aiosqlite.connect(db_path) as db:
            async with db.execute(sql) as cursor:
                rows = await cursor.fetchall()
                col_names = [d[0] for d in cursor.description]

        return {"content": [{"type": "text", "text": f"컬럼: {col_names}\n결과: {rows}"}]}
    except Exception as e:
        return {"content": [{"type": "text", "text": f"쿼리 오류: {e}"}]}
```

**보안 참고**: 프로덕션에서는 ORM(SQLAlchemy 등) 사용 권장

### API 호출 도구

```python
import httpx
from urllib.parse import urlparse

@tool(
    name="call_api",
    description="외부 API 호출 (허용된 도메인만)",
    parameters={"url": str, "method": str, "body": dict | None},
)
async def call_api(args: dict) -> dict:
    url = args.get("url", "")
    method = args.get("method", "GET").upper()
    body = args.get("body")

    # URL 파싱 후 hostname 정확히 검증 (우회 방지)
    allowed_domains = {"api.example.com", "data.example.com"}
    try:
        parsed = urlparse(url)
        if parsed.hostname not in allowed_domains:
            return {"content": [{"type": "text", "text": f"오류: 허용되지 않은 도메인: {parsed.hostname}"}]}
        if parsed.scheme not in ("http", "https"):
            return {"content": [{"type": "text", "text": "오류: http/https만 허용됩니다"}]}
    except Exception:
        return {"content": [{"type": "text", "text": "오류: 유효하지 않은 URL"}]}

    try:
        async with httpx.AsyncClient(timeout=30.0) as client:
            response = await client.request(method, url, json=body)
            # 응답 크기 제한
            text = response.text[:10000] if len(response.text) > 10000 else response.text
            return {"content": [{"type": "text", "text": f"상태: {response.status_code}\n{text}"}]}
    except httpx.TimeoutException:
        return {"content": [{"type": "text", "text": "오류: 요청 타임아웃"}]}
    except Exception as e:
        return {"content": [{"type": "text", "text": f"API 호출 오류: {e}"}]}
```

### 파일 처리 도구

```python
import pandas as pd
from pathlib import Path

@tool(
    name="analyze_csv",
    description="CSV 파일 분석",
    parameters={"file_path": str, "operation": str},
)
async def analyze_csv(args: dict) -> dict:
    file_path = Path(args.get("file_path", ""))
    operation = args.get("operation", "describe")

    if not file_path.exists():
        return {"content": [{"type": "text", "text": "파일 없음"}]}

    df = pd.read_csv(file_path)

    if operation == "describe":
        result = df.describe().to_string()
    elif operation == "head":
        result = df.head(10).to_string()
    elif operation == "columns":
        result = str(df.columns.tolist())
    else:
        result = f"행: {len(df)}, 열: {len(df.columns)}"

    return {"content": [{"type": "text", "text": result}]}
```

## 에러 핸들링

```python
@tool("risky_operation", "위험한 작업", {"input": str})
async def risky_operation(args: dict) -> dict:
    try:
        result = do_risky_thing(args["input"])
        return {"content": [{"type": "text", "text": f"성공: {result}"}]}
    except ValueError as e:
        return {"content": [{"type": "text", "text": f"입력 오류: {e}"}]}
    except Exception as e:
        return {"content": [{"type": "text", "text": f"실행 오류: {e}"}]}
```

## 주의사항

1. **도구 이름**: snake_case, 명확한 동사 사용
2. **설명**: Claude가 이해할 수 있게 명확히
3. **파라미터 타입**: 정확한 타입 힌트
4. **에러 처리**: 항상 유효한 응답 반환
5. **보안**: 입력 검증, 허용 목록 사용
