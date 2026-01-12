# MCP (Model Context Protocol) 연동

## 외부 MCP 서버 연결

```python
options = ClaudeAgentOptions(
    mcp_servers={
        "server-name": {
            "command": "npx",
            "args": ["@example/mcp-server"],
        }
    }
)
```

## 인기 MCP 서버

### Playwright (브라우저 자동화)

```python
options = ClaudeAgentOptions(
    mcp_servers={
        "playwright": {
            "command": "npx",
            "args": ["@playwright/mcp@latest"],
        }
    }
)

# 사용
prompt = "example.com에 접속해서 스크린샷 찍어줘"
```

### Puppeteer

```python
options = ClaudeAgentOptions(
    mcp_servers={
        "puppeteer": {
            "command": "npx",
            "args": ["-y", "@anthropic-ai/mcp-puppeteer"],
        }
    }
)
```

### GitHub

```python
options = ClaudeAgentOptions(
    mcp_servers={
        "github": {
            "command": "npx",
            "args": ["-y", "@anthropic-ai/mcp-github"],
            "env": {
                "GITHUB_TOKEN": os.environ["GITHUB_TOKEN"],
            }
        }
    }
)
```

### PostgreSQL

```python
options = ClaudeAgentOptions(
    mcp_servers={
        "postgres": {
            "command": "npx",
            "args": ["-y", "@anthropic-ai/mcp-postgres"],
            "env": {
                "DATABASE_URL": os.environ["DATABASE_URL"],
            }
        }
    }
)
```

### Slack

```python
options = ClaudeAgentOptions(
    mcp_servers={
        "slack": {
            "command": "npx",
            "args": ["-y", "@anthropic-ai/mcp-slack"],
            "env": {
                "SLACK_BOT_TOKEN": os.environ["SLACK_BOT_TOKEN"],
            }
        }
    }
)
```

## 커스텀 MCP 서버

### Python으로 MCP 서버 구현

```python
# my_mcp_server.py
from mcp import Server, Tool

server = Server("my-server")

@server.tool("my_tool")
async def my_tool(param: str) -> str:
    return f"처리됨: {param}"

if __name__ == "__main__":
    server.run()
```

### 에이전트에 연결

```python
options = ClaudeAgentOptions(
    mcp_servers={
        "my-server": {
            "command": "python",
            "args": ["my_mcp_server.py"],
        }
    }
)
```

## 실전 예시

### 웹 스크래핑 에이전트

```python
async def run_scraping_agent(url: str, task: str):
    options = ClaudeAgentOptions(
        allowed_tools=["Read", "Write"],
        mcp_servers={
            "playwright": {
                "command": "npx",
                "args": ["@playwright/mcp@latest"],
            }
        }
    )

    prompt = f"""
    {url}에서 다음 작업을 수행해줘:
    {task}

    결과를 output.json에 저장해줘.
    """

    async for message in query(prompt=prompt, options=options):
        if hasattr(message, "result"):
            return message.result
```

### 데이터베이스 분석 에이전트

```python
import os

async def run_db_agent(query_task: str):
    options = ClaudeAgentOptions(
        allowed_tools=["Read", "Write"],
        mcp_servers={
            "postgres": {
                "command": "npx",
                "args": ["-y", "@anthropic-ai/mcp-postgres"],
                "env": {
                    "DATABASE_URL": os.environ["DATABASE_URL"],
                }
            }
        }
    )

    prompt = f"""
    데이터베이스에서 다음을 분석해줘:
    {query_task}

    결과를 마크다운 보고서로 작성해줘.
    """

    async for message in query(prompt=prompt, options=options):
        if hasattr(message, "result"):
            return message.result
```

### 멀티 MCP 에이전트

```python
async def run_research_agent(topic: str):
    options = ClaudeAgentOptions(
        allowed_tools=["Read", "Write", "WebSearch"],
        mcp_servers={
            # 브라우저 자동화
            "playwright": {
                "command": "npx",
                "args": ["@playwright/mcp@latest"],
            },
            # GitHub 검색
            "github": {
                "command": "npx",
                "args": ["-y", "@anthropic-ai/mcp-github"],
                "env": {"GITHUB_TOKEN": os.environ["GITHUB_TOKEN"]},
            },
        }
    )

    prompt = f"""
    "{topic}"에 대해 조사해줘:
    1. 웹 검색으로 최신 정보 수집
    2. GitHub에서 관련 프로젝트 찾기
    3. 주요 사이트 방문해서 상세 정보 수집
    4. 종합 보고서 작성
    """

    async for message in query(prompt=prompt, options=options):
        if hasattr(message, "result"):
            return message.result
```

## 환경 변수 관리

```python
import os
from dotenv import load_dotenv

load_dotenv()

def get_mcp_config():
    """환경에 따른 MCP 설정"""
    config = {}

    if os.environ.get("GITHUB_TOKEN"):
        config["github"] = {
            "command": "npx",
            "args": ["-y", "@anthropic-ai/mcp-github"],
            "env": {"GITHUB_TOKEN": os.environ["GITHUB_TOKEN"]},
        }

    if os.environ.get("DATABASE_URL"):
        config["postgres"] = {
            "command": "npx",
            "args": ["-y", "@anthropic-ai/mcp-postgres"],
            "env": {"DATABASE_URL": os.environ["DATABASE_URL"]},
        }

    return config

options = ClaudeAgentOptions(
    mcp_servers=get_mcp_config(),
)
```

## 주의사항

1. **프로세스 관리**: MCP 서버는 별도 프로세스로 실행됨
2. **환경 변수**: 민감 정보는 env로 전달
3. **버전**: `@latest` 또는 특정 버전 명시
4. **종료**: 에이전트 종료 시 MCP 서버도 정리됨
5. **타임아웃**: 느린 MCP 서버는 타임아웃 발생 가능

## MCP 서버 목록

- [modelcontextprotocol/servers](https://github.com/modelcontextprotocol/servers)
- 브라우저: playwright, puppeteer
- 데이터베이스: postgres, sqlite, redis
- 클라우드: aws, gcp, azure
- 커뮤니케이션: slack, discord, gmail
- 개발: github, gitlab, jira
