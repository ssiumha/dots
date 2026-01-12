# query() 함수와 옵션

## 기본 사용법

```python
from claude_agent_sdk import query, ClaudeAgentOptions

async for message in query(
    prompt="작업 지시",
    options=ClaudeAgentOptions(...)
):
    # 메시지 처리
    pass
```

## ClaudeAgentOptions 전체 옵션

| 옵션 | 타입 | 기본값 | 설명 |
|------|------|--------|------|
| `allowed_tools` | `list[str]` | 모두 | 허용할 도구 목록 |
| `cwd` | `str` | 현재 디렉토리 | 작업 디렉토리 |
| `permission_mode` | `str` | `"default"` | 권한 모드 |
| `system_prompt` | `str` | None | 추가 시스템 프롬프트 |
| `max_turns` | `int` | None | 최대 턴 수 |
| `hooks` | `dict` | None | 훅 설정 |
| `agents` | `dict` | None | 서브에이전트 정의 |
| `custom_tools` | `list` | None | 커스텀 도구 서버 |
| `mcp_servers` | `dict` | None | MCP 서버 설정 |
| `resume` | `str` | None | 재개할 세션 ID |
| `cli_path` | `str` | 번들 CLI | CLI 경로 |
| `model` | `str` | sonnet | 모델 선택 |

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

## permission_mode

| 모드 | 설명 | 용도 |
|------|------|------|
| `default` | 각 도구 사용 시 확인 | 개발/테스트 |
| `acceptEdits` | 파일 수정 자동 승인 | 자동화 |
| `bypassPermissions` | 모든 권한 자동 승인 | 프로덕션 (주의) |

## 메시지 타입

```python
async for message in query(...):
    # 초기화 메시지 (세션 ID 포함)
    if hasattr(message, "subtype") and message.subtype == "init":
        session_id = message.session_id

    # 어시스턴트 메시지
    if hasattr(message, "content"):
        for block in message.content:
            if hasattr(block, "text"):
                print(block.text)  # 텍스트
            if getattr(block, "type", None) == "tool_use":
                print(block.name, block.input)  # 도구 호출

    # 최종 결과
    if hasattr(message, "result"):
        print(message.result)
```

## 예시

### 읽기 전용 에이전트

```python
options = ClaudeAgentOptions(
    allowed_tools=["Read", "Glob", "Grep"],
    permission_mode="bypassPermissions",  # 읽기만 하므로 안전
)
```

### 코드 수정 에이전트

```python
options = ClaudeAgentOptions(
    allowed_tools=["Read", "Edit", "Write", "Bash", "Glob", "Grep"],
    cwd="/path/to/project",
    permission_mode="acceptEdits",
    max_turns=50,  # 복잡한 작업용
)
```

### 웹 검색 에이전트

```python
options = ClaudeAgentOptions(
    allowed_tools=["WebSearch", "WebFetch", "Read", "Write"],
    system_prompt="최신 정보를 검색하고 보고서를 작성하세요.",
)
```
