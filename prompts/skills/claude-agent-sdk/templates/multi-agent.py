#!/usr/bin/env python3
"""
서브에이전트가 포함된 Claude Agent SDK 에이전트

사용법:
    python agent.py "코드를 리뷰하고 테스트해줘"

서브에이전트:
    - 메인 에이전트가 작업을 위임
    - 각 서브에이전트는 독립 컨텍스트
    - 병렬 실행 가능 (최대 10개)
"""
import asyncio
import argparse
from claude_agent_sdk import query, ClaudeAgentOptions, AgentDefinition


# === 서브에이전트 정의 ===

AGENTS = {
    "code-reviewer": AgentDefinition(
        description="코드 품질, 보안, 성능을 리뷰합니다. 코드 리뷰 요청 시 사용.",
        prompt="""당신은 시니어 코드 리뷰어입니다.

## 리뷰 관점
1. 코드 품질: 가독성, 유지보수성, SOLID 원칙
2. 보안: 인젝션, XSS, 민감정보 노출
3. 성능: 불필요한 연산, N+1, 메모리 누수

## 출력 형식
### 요약
[전체 평가 1-2문장]

### 이슈
- [심각도] 파일:라인 - 설명

### 개선 제안
- [우선순위] 제안 내용
""",
        tools=["Read", "Glob", "Grep"],  # 읽기 전용
        model="sonnet",
    ),
    "test-runner": AgentDefinition(
        description="테스트를 실행하고 결과를 분석합니다. 테스트 실행 요청 시 사용.",
        prompt="""당신은 테스트 전문가입니다.

## 작업
1. 테스트 파일 탐색
2. 테스트 실행 (pytest, jest 등)
3. 결과 분석 및 요약

## 출력 형식
### 테스트 결과
- 통과: N개
- 실패: N개
- 스킵: N개

### 실패 분석
[실패한 테스트별 원인 분석]
""",
        tools=["Read", "Bash", "Glob", "Grep"],
        model="haiku",  # 빠른 실행
    ),
    "doc-generator": AgentDefinition(
        description="코드에서 문서를 생성합니다. 문서화 요청 시 사용.",
        prompt="""당신은 기술 문서 작성자입니다.

## 작업
1. 코드 구조 분석
2. 함수/클래스 시그니처 추출
3. 문서 생성 (docstring, README)

## 출력 형식
마크다운 형식의 문서
""",
        tools=["Read", "Glob", "Grep"],
        model="sonnet",
    ),
}


# === 에이전트 실행 ===

async def run_agent(prompt: str, cwd: str | None = None):
    """서브에이전트가 포함된 에이전트 실행"""
    options = ClaudeAgentOptions(
        # Task 도구 필수 (서브에이전트 호출용)
        allowed_tools=["Read", "Edit", "Bash", "Glob", "Grep", "Task"],
        cwd=cwd,
        permission_mode="acceptEdits",
        # 서브에이전트 등록
        agents=AGENTS,
    )

    async for message in query(prompt=prompt, options=options):
        # 서브에이전트 호출 감지
        if hasattr(message, "content"):
            for block in message.content:
                if hasattr(block, "text"):
                    print(block.text)
                if getattr(block, "type", None) == "tool_use" and block.name == "Task":
                    agent_type = block.input.get("subagent_type", "unknown")
                    print(f"\n[서브에이전트 호출] {agent_type}")

        # 서브에이전트 컨텍스트 내 메시지
        if hasattr(message, "parent_tool_use_id") and message.parent_tool_use_id:
            print("  (서브에이전트 실행 중)")

        if hasattr(message, "result"):
            print("\n=== 결과 ===")
            print(message.result)
            return message.result

    return None


async def run_parallel_agents(prompts: list[tuple[str, str]], cwd: str | None = None):
    """여러 서브에이전트 병렬 실행 예시.

    Args:
        prompts: (에이전트명, 프롬프트) 튜플 리스트
        cwd: 작업 디렉토리

    Returns:
        각 에이전트 실행 결과 리스트 (예외 포함)

    Example:
        prompts = [
            ("code-reviewer", "src/ 리뷰"),
            ("test-runner", "테스트 실행"),
        ]
        results = await run_parallel_agents(prompts)

    Note:
        이 함수는 예시 목적입니다.
        CLI에서 사용하려면 --parallel 옵션 추가 필요.
    """
    tasks = []
    for agent_name, prompt in prompts:
        task = run_agent(f"{agent_name} 에이전트로 {prompt}", cwd)
        tasks.append(task)

    results = await asyncio.gather(*tasks, return_exceptions=True)
    return results


def main() -> int:
    """CLI 엔트리 포인트.

    Returns:
        종료 코드 (0: 성공, 1: 실패)
    """
    parser = argparse.ArgumentParser(description="Claude Agent with Subagents")
    parser.add_argument("prompt", help="에이전트에게 전달할 작업 지시")
    parser.add_argument("--cwd", help="작업 디렉토리", default=None)
    args = parser.parse_args()

    result = asyncio.run(run_agent(args.prompt, args.cwd))
    return 0 if result else 1


if __name__ == "__main__":
    exit(main())
