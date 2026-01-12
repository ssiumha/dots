# Claude Agent SDK Reference

Python 기반 독립 실행형 AI 에이전트 구축을 위한 SDK.

## 개요

Claude Agent SDK는 Claude Code를 구동하는 동일한 엔진을 라이브러리로 제공합니다.
파일 읽기/쓰기, 명령 실행, 웹 검색 등을 자율적으로 수행하는 에이전트를 구축할 수 있습니다.

## 설치

```bash
pip install claude-agent-sdk
export ANTHROPIC_API_KEY=your-api-key
```

## 핵심 구성요소

| 구성요소 | 설명 | 리소스 |
|----------|------|--------|
| `query()` | 메인 에이전트 실행 함수 | `02-query-options.md` |
| `ClaudeAgentOptions` | 에이전트 설정 | `02-query-options.md` |
| `@tool` | 커스텀 도구 정의 | `03-custom-tools.md` |
| `HookMatcher` | 훅 매칭 | `04-hooks.md` |
| `AgentDefinition` | 서브에이전트 정의 | `05-subagents.md` |

## 빌트인 도구

| 도구 | 설명 |
|------|------|
| `Read` | 파일 읽기 |
| `Write` | 파일 생성 |
| `Edit` | 파일 수정 |
| `Bash` | 명령어 실행 |
| `Glob` | 파일 패턴 검색 |
| `Grep` | 내용 검색 |
| `WebSearch` | 웹 검색 |
| `WebFetch` | 웹 페이지 가져오기 |
| `Task` | 서브에이전트 호출 |
| `AskUserQuestion` | 사용자 질문 |

## 템플릿

| 템플릿 | 용도 | 포함 기능 |
|--------|------|----------|
| `basic-agent.py` | 최소 시작 | query, 기본 옵션 |
| `tool-agent.py` | 커스텀 도구 | @tool, MCP 서버 |
| `hook-agent.py` | 검증/로깅 | PreToolUse, PostToolUse |
| `multi-agent.py` | 서브에이전트 | AgentDefinition, 병렬 실행 |
| `production-agent.py` | 프로덕션 | 전체 (훅+세션+에러핸들링) |

## 리소스 구조

```
resources/
├── 01-setup.md           # 설치, pyproject.toml
├── 02-query-options.md   # query() 함수, ClaudeAgentOptions
├── 03-custom-tools.md    # @tool, create_sdk_mcp_server
├── 04-hooks.md           # HookMatcher, 훅 패턴
├── 05-subagents.md       # AgentDefinition, 병렬 실행
├── 06-sessions.md        # 세션 관리, 재개
└── 07-mcp.md             # 외부 MCP 서버 연동
```

## 권한 모드

| 모드 | 설명 | 용도 |
|------|------|------|
| `default` | 각 도구 사용 시 확인 | 개발/테스트 |
| `acceptEdits` | 파일 수정 자동 승인 | 자동화 |
| `bypassPermissions` | 모든 권한 자동 승인 | 프로덕션 (주의) |

## vs Claude Code Subagent

| 항목 | Claude Code Subagent | Agent SDK |
|------|---------------------|-----------|
| 결과물 | `.claude/agents/*.md` | `*.py` 파일 |
| 실행 환경 | Claude Code 내부 | 독립 Python 앱 |
| 용도 | 작업 위임 | 프로덕션 자동화 |
| 정의 방식 | 마크다운 | Python 코드 |

## 공식 문서

- [SDK Overview](https://platform.claude.com/docs/en/agent-sdk/overview)
- [Python SDK](https://github.com/anthropics/claude-agent-sdk-python)
- [TypeScript SDK](https://github.com/anthropics/claude-agent-sdk-typescript)
- [MCP Servers](https://github.com/modelcontextprotocol/servers)

## 학습 순서

1. **시작**: `01-setup.md` → `templates/basic-agent.py`
2. **커스텀 도구**: `03-custom-tools.md` → `templates/tool-agent.py`
3. **안전성**: `04-hooks.md` → `templates/hook-agent.py`
4. **확장**: `05-subagents.md` → `templates/multi-agent.py`
5. **프로덕션**: `06-sessions.md` + `07-mcp.md` → `templates/production-agent.py`
