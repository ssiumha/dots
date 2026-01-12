#!/usr/bin/env python3
"""
커스텀 도구가 포함된 Claude Agent SDK 에이전트

사용법:
    python agent.py "데이터베이스에서 사용자 검색해줘"
"""
import asyncio
import argparse
import sys
from claude_agent_sdk import (
    query,
    ClaudeAgentOptions,
    tool,
    create_sdk_mcp_server,
)


# === 커스텀 도구 정의 ===

@tool(
    name="search_database",
    description="데이터베이스에서 검색합니다",
    parameters={"query": str, "limit": int},
)
async def search_database(args: dict) -> dict:
    """데이터베이스 검색 도구

    Args:
        args: {"query": 검색어, "limit": 결과 제한 (1-1000)}

    Returns:
        검색 결과 또는 에러 메시지
    """
    try:
        query_str = args.get("query", "")
        if not query_str:
            return {"content": [{"type": "text", "text": "오류: 검색어가 비어있습니다"}]}

        # limit 범위 검증 (1-1000)
        limit = min(max(args.get("limit", 10), 1), 1000)

        # TODO: 실제 DB 검색 로직 구현
        results = [
            {"id": 1, "name": "Example 1"},
            {"id": 2, "name": "Example 2"},
        ][:limit]

        return {
            "content": [
                {
                    "type": "text",
                    "text": f"검색 결과 ({len(results)}건):\n{results}",
                }
            ]
        }
    except Exception as e:
        return {"content": [{"type": "text", "text": f"검색 오류: {e}"}]}


@tool(
    name="send_notification",
    description="알림을 전송합니다",
    parameters={"message": str, "channel": str},
)
async def send_notification(args: dict) -> dict:
    """알림 전송 도구"""
    try:
        message = args.get("message", "")
        if not message:
            return {"content": [{"type": "text", "text": "오류: 메시지가 비어있습니다"}]}

        channel = args.get("channel", "default")

        # TODO: 실제 알림 로직 구현
        print(f"[{channel}] {message}")

        return {
            "content": [
                {
                    "type": "text",
                    "text": f"알림 전송 완료: {channel}",
                }
            ]
        }
    except Exception as e:
        return {"content": [{"type": "text", "text": f"알림 전송 오류: {e}"}]}


# === MCP 서버 생성 ===

def create_tools_server():
    """커스텀 도구를 MCP 서버로 래핑"""
    return create_sdk_mcp_server(
        name="custom-tools",
        version="1.0.0",
        tools=[search_database, send_notification],
    )


# === 에이전트 실행 ===

async def run_agent(prompt: str):
    """커스텀 도구가 포함된 에이전트 실행"""
    tools_server = create_tools_server()

    options = ClaudeAgentOptions(
        # 빌트인 도구 + 커스텀 도구
        allowed_tools=["Read", "Glob", "Grep"],
        # 커스텀 도구 서버
        custom_tools=[tools_server],
        permission_mode="acceptEdits",
    )

    try:
        async for message in query(prompt=prompt, options=options):
            if hasattr(message, "content"):
                for block in message.content:
                    if hasattr(block, "text"):
                        print(block.text)
                    # 도구 사용 로깅
                    if getattr(block, "type", None) == "tool_use":
                        print(f"[도구 호출] {block.name}: {block.input}")

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
    parser = argparse.ArgumentParser(description="Claude Agent with Custom Tools")
    parser.add_argument("prompt", help="에이전트에게 전달할 작업 지시")
    args = parser.parse_args()

    result = asyncio.run(run_agent(args.prompt))
    return 0 if result else 1


if __name__ == "__main__":
    exit(main())
