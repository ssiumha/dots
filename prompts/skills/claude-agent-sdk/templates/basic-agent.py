#!/usr/bin/env python3
"""
최소 Claude Agent SDK 에이전트

사용법:
    python agent.py "작업 지시"
    python agent.py --cwd /path/to/project "작업 지시"
"""
import asyncio
import argparse
import sys
from claude_agent_sdk import query, ClaudeAgentOptions


async def run_agent(prompt: str, cwd: str | None = None):
    """에이전트 실행

    Args:
        prompt: 에이전트에게 전달할 작업 지시
        cwd: 작업 디렉토리 (None이면 현재 디렉토리)

    Returns:
        실행 결과 문자열 또는 None (실패 시)
    """
    options = ClaudeAgentOptions(
        # 허용할 도구 (필요한 것만 추가)
        allowed_tools=["Read", "Glob", "Grep"],
        # 작업 디렉토리
        cwd=cwd,
        # 권한 모드: default, acceptEdits, bypassPermissions
        permission_mode="default",
    )

    try:
        async for message in query(prompt=prompt, options=options):
            # 진행 상황 출력
            if hasattr(message, "content"):
                for block in message.content:
                    if hasattr(block, "text"):
                        print(block.text)

            # 최종 결과
            if hasattr(message, "result"):
                print("\n=== 결과 ===")
                print(message.result)
                return message.result

    except KeyboardInterrupt:
        print("\n작업이 취소되었습니다.", file=sys.stderr)
    except Exception as e:
        print(f"\n에러 발생: {e}", file=sys.stderr)

    return None


def main():
    parser = argparse.ArgumentParser(description="Claude Agent")
    parser.add_argument("prompt", help="에이전트에게 전달할 작업 지시")
    parser.add_argument("--cwd", help="작업 디렉토리", default=None)
    args = parser.parse_args()

    result = asyncio.run(run_agent(args.prompt, args.cwd))
    return 0 if result else 1


if __name__ == "__main__":
    exit(main())
