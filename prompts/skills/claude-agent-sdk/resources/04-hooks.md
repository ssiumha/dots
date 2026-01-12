# 훅 (Hooks)

## 훅 종류

| 훅 | 시점 | 용도 |
|----|------|------|
| `PreToolUse` | 도구 실행 전 | 검증, 차단 |
| `PostToolUse` | 도구 실행 후 | 로깅, 감사 |
| `Stop` | 에이전트 종료 시 | 정리, 알림 |
| `SessionStart` | 세션 시작 시 | 초기화 |
| `SessionEnd` | 세션 종료 시 | 정리 |
| `UserPromptSubmit` | 프롬프트 제출 시 | 검증 |

## 훅 함수 시그니처

```python
async def hook_function(
    input_data: dict,    # 도구 입력 정보
    tool_use_id: str,    # 도구 사용 ID
    context: dict,       # 컨텍스트 정보
) -> dict:
    # 처리 로직...
    return {}  # 또는 {"decision": "block", "reason": "..."}
```

## HookMatcher

```python
from claude_agent_sdk import HookMatcher

# 특정 도구에만 적용
HookMatcher(matcher="Bash", hooks=[validate_bash])

# 여러 도구에 적용 (정규식)
HookMatcher(matcher="Edit|Write", hooks=[log_file_change])

# 모든 도구에 적용
HookMatcher(matcher=".*", hooks=[log_all_tools])
```

## 실전 예시

### 위험한 명령어 차단

```python
DANGEROUS_PATTERNS = [
    "rm -rf /",
    "rm -rf ~",
    ":(){ :|:& };:",  # fork bomb
    "> /dev/sda",
    "mkfs.",
    "dd if=/dev/zero",
    "chmod -R 777 /",
]

async def block_dangerous_bash(input_data: dict, tool_use_id: str, context: dict) -> dict:
    """위험한 Bash 명령어 차단"""
    command = input_data.get("tool_input", {}).get("command", "")

    for pattern in DANGEROUS_PATTERNS:
        if pattern in command:
            return {
                "decision": "block",
                "reason": f"위험한 명령어 패턴: {pattern}",
            }

    return {}
```

### 민감 파일 보호

```python
import fnmatch
from pathlib import Path

PROTECTED_PATTERNS = [
    "*.env",           # .env, .env.local 등
    "*credentials*",   # credentials.json 등
    "*secrets*",       # secrets.yaml 등
    ".git/config",
    "*id_rsa*",
    "*.pem",
    "*.key",
    "*.p12",
]

async def protect_sensitive_files(input_data: dict, tool_use_id: str, context: dict) -> dict:
    """민감한 파일 접근 차단 (fnmatch로 정확한 패턴 매칭)"""
    tool_name = input_data.get("tool_name", "")
    file_path = input_data.get("tool_input", {}).get("file_path", "")

    if tool_name in ["Edit", "Write", "Read"]:
        # 파일명만 추출하여 패턴 매칭
        filename = Path(file_path).name

        for pattern in PROTECTED_PATTERNS:
            # 전체 경로와 파일명 모두 검사
            if fnmatch.fnmatch(file_path, pattern) or fnmatch.fnmatch(filename, pattern):
                return {"decision": "block", "reason": f"보호된 파일: {pattern}"}

    return {}
```

### 감사 로깅

```python
import json
from datetime import datetime
from pathlib import Path

AUDIT_LOG = Path("./audit.log")

async def audit_log(input_data: dict, tool_use_id: str, context: dict) -> dict:
    """모든 도구 사용 로깅"""
    entry = {
        "timestamp": datetime.now().isoformat(),
        "tool": input_data.get("tool_name"),
        "input": input_data.get("tool_input"),
        "tool_use_id": tool_use_id,
    }

    with open(AUDIT_LOG, "a") as f:
        f.write(json.dumps(entry, ensure_ascii=False) + "\n")

    return {}
```

### 실행 시간 제한

```python
import asyncio

async def timeout_bash(input_data: dict, tool_use_id: str, context: dict) -> dict:
    """Bash 명령 타임아웃 설정"""
    command = input_data.get("tool_input", {}).get("command", "")

    # 장시간 실행 명령어 경고
    long_running = ["npm install", "pip install", "docker build", "make"]
    for pattern in long_running:
        if pattern in command:
            # 경고만 하고 허용 (차단하려면 decision: block)
            print(f"[경고] 장시간 실행 가능: {pattern}")

    return {}
```

### 비용 제어

```python
TOOL_COUNTS = {}
MAX_TOOL_CALLS = {"WebSearch": 10, "WebFetch": 20}

async def limit_tool_calls(input_data: dict, tool_use_id: str, context: dict) -> dict:
    """도구 호출 횟수 제한"""
    tool_name = input_data.get("tool_name", "")

    if tool_name in MAX_TOOL_CALLS:
        TOOL_COUNTS[tool_name] = TOOL_COUNTS.get(tool_name, 0) + 1

        if TOOL_COUNTS[tool_name] > MAX_TOOL_CALLS[tool_name]:
            return {
                "decision": "block",
                "reason": f"{tool_name} 호출 한도 초과 ({MAX_TOOL_CALLS[tool_name]}회)",
            }

    return {}
```

## 옵션에 훅 설정

```python
options = ClaudeAgentOptions(
    hooks={
        "PreToolUse": [
            HookMatcher(matcher="Bash", hooks=[block_dangerous_bash, timeout_bash]),
            HookMatcher(matcher="Edit|Write|Read", hooks=[protect_sensitive_files]),
            HookMatcher(matcher="WebSearch|WebFetch", hooks=[limit_tool_calls]),
        ],
        "PostToolUse": [
            HookMatcher(matcher=".*", hooks=[audit_log]),
        ],
    },
)
```

## 주의사항

1. **훅 순서**: 배열 순서대로 실행
2. **차단 시**: `{"decision": "block"}` 반환하면 즉시 중단
3. **에러 처리**: 훅 내 예외는 에이전트 중단 유발
4. **성능**: 훅은 동기적으로 실행되므로 무거운 작업 지양
