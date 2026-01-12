# 프로젝트 설정

## 설치

```bash
pip install claude-agent-sdk
```

**요구사항**:
- Python 3.10+
- Claude Code CLI (자동 번들됨)

## pyproject.toml

```toml
[project]
name = "my-agent"
version = "0.1.0"
requires-python = ">=3.10"
dependencies = [
    "claude-agent-sdk>=0.1.19",
]

[project.optional-dependencies]
dev = [
    "pytest>=8.0",
    "pytest-asyncio>=0.23",
]

[project.scripts]
my-agent = "my_agent.agent:main"
```

## 환경 변수

```bash
# API 키 (필수)
export ANTHROPIC_API_KEY=your-api-key

# 또는 다른 프로바이더
export CLAUDE_CODE_USE_BEDROCK=1    # Amazon Bedrock
export CLAUDE_CODE_USE_VERTEX=1     # Google Vertex AI
export CLAUDE_CODE_USE_FOUNDRY=1    # Microsoft Foundry
```

## 프로젝트 구조

```
my-agent/
├── pyproject.toml
├── src/
│   └── my_agent/
│       ├── __init__.py
│       ├── agent.py      # 메인 에이전트
│       ├── tools.py      # 커스텀 도구
│       └── hooks.py      # 훅 정의
├── tests/
│   └── test_agent.py
└── logs/                 # 런타임 로그
```

## 테스트

```python
# tests/test_agent.py
import pytest
from my_agent.agent import run_agent

@pytest.mark.asyncio
async def test_agent_basic():
    result = await run_agent("2 + 2는?")
    assert result is not None
```

```bash
pytest tests/ -v
```
