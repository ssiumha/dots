#!/usr/bin/env python3
"""
프로덕션용 Claude Agent SDK 에이전트

포함 기능:
    - 세션 관리 (재개, 포크)
    - 훅 (검증, 로깅)
    - 에러 핸들링
    - 구조화된 출력

사용법:
    python agent.py "작업 지시"
    python agent.py --resume <session_id> "계속해줘"
    python agent.py --output json "작업 지시"
"""
import asyncio
import argparse
import json
import logging
import sys
from datetime import datetime
from pathlib import Path
from dataclasses import dataclass, asdict
from claude_agent_sdk import (
    query,
    ClaudeAgentOptions,
    HookMatcher,
    AgentDefinition,
)

# === 설정 ===

LOG_DIR = Path("./logs")

# 로그 디렉토리 생성 (실패 시 stdout만 사용)
try:
    LOG_DIR.mkdir(exist_ok=True)
    _log_handlers = [
        logging.FileHandler(LOG_DIR / "agent.log"),
        logging.StreamHandler(),
    ]
except OSError as e:
    print(f"[경고] 로그 디렉토리 생성 실패: {e}", file=sys.stderr)
    _log_handlers = [logging.StreamHandler()]

logging.basicConfig(
    level=logging.INFO,
    format="%(asctime)s | %(levelname)s | %(message)s",
    handlers=_log_handlers,
)
logger = logging.getLogger(__name__)


# === 데이터 모델 ===

@dataclass
class AgentResult:
    """에이전트 실행 결과"""
    success: bool
    session_id: str | None
    result: str | None
    error: str | None
    duration_seconds: float
    tools_used: list[str]

    def to_json(self) -> str:
        return json.dumps(asdict(self), ensure_ascii=False, indent=2)


# === 훅 ===

async def log_tool_use(input_data: dict, tool_use_id: str, context: dict) -> dict:
    """도구 사용 로깅"""
    tool_name = input_data.get("tool_name", "unknown")
    logger.info(f"Tool: {tool_name}")
    return {}


async def validate_dangerous_ops(input_data: dict, tool_use_id: str, context: dict) -> dict:
    """위험 작업 차단 (에러 발생 시 허용으로 폴백).

    Args:
        input_data: 도구 입력 정보
        tool_use_id: 도구 사용 ID
        context: 컨텍스트 정보

    Returns:
        빈 dict (허용) 또는 {"decision": "block", "reason": str} (차단)

    Note:
        훅 내 예외는 에이전트를 중단시키므로 fail-open으로 처리.
    """
    try:
        tool_name = input_data.get("tool_name", "")
        tool_input = input_data.get("tool_input", {})

        # Bash 위험 명령어
        if tool_name == "Bash":
            cmd = tool_input.get("command", "")
            dangerous = ["rm -rf /", "rm -rf ~", "> /dev/sda", "mkfs."]
            for pattern in dangerous:
                if pattern in cmd:
                    logger.warning(f"Blocked dangerous command: {cmd}")
                    return {"decision": "block", "reason": f"위험한 명령어: {pattern}"}

        # 민감 파일 수정
        if tool_name in ["Edit", "Write"]:
            path = tool_input.get("file_path", "")
            protected = [".env", "credentials", "secrets"]
            for pattern in protected:
                if pattern in path:
                    logger.warning(f"Blocked protected file: {path}")
                    return {"decision": "block", "reason": f"보호된 파일: {pattern}"}

        return {}
    except Exception as e:
        logger.error(f"Hook validation error: {e}", exc_info=True)
        return {}  # fail-open: 검증 실패 시 허용


# === 서브에이전트 ===

AGENTS = {
    "analyzer": AgentDefinition(
        description="코드/데이터 분석용. 분석 요청 시 사용.",
        prompt="당신은 분석 전문가입니다. 요청된 내용을 분석하고 인사이트를 제공하세요.",
        tools=["Read", "Glob", "Grep"],
        model="sonnet",
    ),
}


# === 에이전트 ===

class ProductionAgent:
    """프로덕션용 에이전트"""

    def __init__(
        self,
        cwd: str | None = None,
        resume: str | None = None,
    ):
        self.cwd = cwd
        self.resume = resume
        self.session_id: str | None = None
        self.tools_used: list[str] = []

    def _create_options(self) -> ClaudeAgentOptions:
        """옵션 생성"""
        opts = ClaudeAgentOptions(
            allowed_tools=["Read", "Edit", "Write", "Bash", "Glob", "Grep", "Task"],
            cwd=self.cwd,
            permission_mode="acceptEdits",
            agents=AGENTS,
            hooks={
                "PreToolUse": [
                    HookMatcher(matcher=".*", hooks=[validate_dangerous_ops]),
                ],
                "PostToolUse": [
                    HookMatcher(matcher=".*", hooks=[log_tool_use]),
                ],
            },
        )

        # 세션 재개
        if self.resume:
            opts.resume = self.resume

        return opts

    async def run(self, prompt: str) -> AgentResult:
        """에이전트 실행"""
        start_time = datetime.now()
        result_text = None
        error_text = None

        try:
            logger.info(f"Starting agent: {prompt[:50]}...")

            async for message in query(prompt=prompt, options=self._create_options()):
                # 세션 ID 캡처
                if hasattr(message, "subtype") and message.subtype == "init":
                    self.session_id = message.session_id
                    logger.info(f"Session: {self.session_id}")

                # 도구 사용 추적
                if hasattr(message, "content"):
                    for block in message.content:
                        if getattr(block, "type", None) == "tool_use":
                            self.tools_used.append(block.name)
                        if hasattr(block, "text"):
                            print(block.text)

                # 결과
                if hasattr(message, "result"):
                    result_text = message.result

            logger.info("Agent completed successfully")

        except Exception as e:
            error_text = str(e)
            logger.error(f"Agent error: {error_text}")

        duration = (datetime.now() - start_time).total_seconds()

        return AgentResult(
            success=error_text is None,
            session_id=self.session_id,
            result=result_text,
            error=error_text,
            duration_seconds=duration,
            tools_used=list(set(self.tools_used)),
        )


# === CLI ===

def main():
    parser = argparse.ArgumentParser(description="Production Claude Agent")
    parser.add_argument("prompt", help="작업 지시")
    parser.add_argument("--cwd", help="작업 디렉토리")
    parser.add_argument("--resume", help="재개할 세션 ID")
    parser.add_argument(
        "--output",
        choices=["text", "json"],
        default="text",
        help="출력 형식",
    )
    args = parser.parse_args()

    agent = ProductionAgent(cwd=args.cwd, resume=args.resume)
    result = asyncio.run(agent.run(args.prompt))

    if args.output == "json":
        print(result.to_json())
    else:
        if result.success:
            print("\n=== 결과 ===")
            print(result.result)
            print(f"\n(세션 ID: {result.session_id})")
        else:
            print(f"\n에러: {result.error}")

    return 0 if result.success else 1


if __name__ == "__main__":
    exit(main())
