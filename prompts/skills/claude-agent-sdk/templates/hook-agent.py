#!/usr/bin/env python3
"""
훅이 포함된 Claude Agent SDK 에이전트

사용법:
    python agent.py "코드 리팩토링해줘"

훅 종류:
    - PreToolUse: 도구 실행 전 검증/차단
    - PostToolUse: 도구 실행 후 로깅/감사
    - Stop: 에이전트 종료 시
    - SessionStart/SessionEnd: 세션 시작/종료 시
"""
import asyncio
import argparse
import fnmatch
import re
import sys
from datetime import datetime
from pathlib import Path
from claude_agent_sdk import query, ClaudeAgentOptions, HookMatcher


# === 감사 로그 ===

AUDIT_LOG = Path("./audit.log")


def log_audit(action: str, details: str) -> None:
    """감사 로그 기록 (실패해도 에이전트 실행 중단 안 함)"""
    try:
        timestamp = datetime.now().isoformat()
        with open(AUDIT_LOG, "a") as f:
            f.write(f"{timestamp} | {action} | {details}\n")
    except IOError as e:
        print(f"[경고] 로그 기록 실패: {e}", file=sys.stderr)


# === PreToolUse 훅 ===

# 성능 최적화: 정규식 사전 컴파일 (모듈 로드 시 1회)
_DANGEROUS_BASH_PATTERNS = [
    (re.compile(r"rm\s+-[rf]{2}\s+[/~]", re.IGNORECASE), "rm -rf / 또는 ~"),
    (re.compile(r":\s*\(\s*\)\s*\{.*\|.*&\s*\}\s*;", re.IGNORECASE), "fork bomb"),
    (re.compile(r">\s*/dev/sd[a-z]", re.IGNORECASE), "> /dev/sdX"),
    (re.compile(r"mkfs\.", re.IGNORECASE), "mkfs.*"),
    (re.compile(r"dd\s+if=/dev/zero", re.IGNORECASE), "dd if=/dev/zero"),
    (re.compile(r"chmod\s+-R\s+777\s+/", re.IGNORECASE), "chmod -R 777 /"),
]


async def validate_bash_command(input_data: dict, tool_use_id: str, context: dict) -> dict:
    """위험한 Bash 명령어 차단 (정규식 기반으로 우회 방지).

    Args:
        input_data: 도구 입력 정보 {"tool_input": {"command": str}}
        tool_use_id: 도구 사용 ID
        context: 컨텍스트 정보

    Returns:
        빈 dict (허용) 또는 {"decision": "block", "reason": str} (차단)
    """
    tool_input = input_data.get("tool_input", {})
    command = tool_input.get("command", "")

    for pattern, description in _DANGEROUS_BASH_PATTERNS:
        if pattern.search(command):
            log_audit("BLOCKED", f"Dangerous command: {command}")
            return {
                "decision": "block",
                "reason": f"위험한 명령어 패턴 감지: {description}",
            }

    return {}  # 허용


# 보호할 파일 패턴 (fnmatch 형식)
_PROTECTED_FILE_PATTERNS = [
    "*.env",           # .env, .env.local 등
    "*credentials*",   # credentials.json 등
    "*secrets*",       # secrets.yaml 등
    ".git/config",
    "*id_rsa*",
    "*.pem",
    "*.key",
]


async def validate_file_edit(input_data: dict, tool_use_id: str, context: dict) -> dict:
    """민감한 파일 수정 차단 (fnmatch 패턴 매칭).

    Args:
        input_data: 도구 입력 정보 {"tool_input": {"file_path": str}}
        tool_use_id: 도구 사용 ID
        context: 컨텍스트 정보

    Returns:
        빈 dict (허용) 또는 {"decision": "block", "reason": str} (차단)
    """
    tool_input = input_data.get("tool_input", {})
    file_path = tool_input.get("file_path", "")
    filename = Path(file_path).name

    for pattern in _PROTECTED_FILE_PATTERNS:
        if fnmatch.fnmatch(file_path, pattern) or fnmatch.fnmatch(filename, pattern):
            log_audit("BLOCKED", f"Protected file: {file_path}")
            return {
                "decision": "block",
                "reason": f"보호된 파일 수정 불가: {pattern}",
            }

    return {}


# === PostToolUse 훅 ===

async def log_file_changes(input_data: dict, tool_use_id: str, context: dict) -> dict:
    """파일 변경 로깅"""
    tool_name = input_data.get("tool_name", "")
    tool_input = input_data.get("tool_input", {})

    if tool_name in ["Edit", "Write"]:
        file_path = tool_input.get("file_path", "unknown")
        log_audit("FILE_CHANGED", file_path)

    return {}


async def log_command_execution(input_data: dict, tool_use_id: str, context: dict) -> dict:
    """명령어 실행 로깅"""
    tool_name = input_data.get("tool_name", "")
    tool_input = input_data.get("tool_input", {})

    if tool_name == "Bash":
        command = tool_input.get("command", "unknown")
        log_audit("COMMAND_EXECUTED", command[:100])  # 100자까지

    return {}


# === 에이전트 실행 ===

async def run_agent(prompt: str, cwd: str | None = None):
    """훅이 포함된 에이전트 실행"""
    options = ClaudeAgentOptions(
        allowed_tools=["Read", "Edit", "Write", "Bash", "Glob", "Grep"],
        cwd=cwd,
        permission_mode="acceptEdits",
        hooks={
            # 도구 실행 전 검증
            "PreToolUse": [
                HookMatcher(matcher="Bash", hooks=[validate_bash_command]),
                HookMatcher(matcher="Edit|Write", hooks=[validate_file_edit]),
            ],
            # 도구 실행 후 로깅
            "PostToolUse": [
                HookMatcher(matcher="Edit|Write", hooks=[log_file_changes]),
                HookMatcher(matcher="Bash", hooks=[log_command_execution]),
            ],
        },
    )

    log_audit("SESSION_START", f"Prompt: {prompt[:50]}...")

    try:
        async for message in query(prompt=prompt, options=options):
            if hasattr(message, "content"):
                for block in message.content:
                    if hasattr(block, "text"):
                        print(block.text)

            if hasattr(message, "result"):
                print("\n=== 결과 ===")
                print(message.result)
                log_audit("SESSION_END", "Success")
                return message.result

    except Exception as e:
        log_audit("SESSION_ERROR", str(e))
        raise

    return None


def main():
    parser = argparse.ArgumentParser(description="Claude Agent with Hooks")
    parser.add_argument("prompt", help="에이전트에게 전달할 작업 지시")
    parser.add_argument("--cwd", help="작업 디렉토리", default=None)
    args = parser.parse_args()

    result = asyncio.run(run_agent(args.prompt, args.cwd))
    return 0 if result else 1


if __name__ == "__main__":
    exit(main())
