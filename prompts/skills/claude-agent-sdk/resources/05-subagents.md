# 서브에이전트

## AgentDefinition

```python
from claude_agent_sdk import AgentDefinition

agent = AgentDefinition(
    description="언제 사용할지 설명",  # Claude가 자동 위임 결정에 사용
    prompt="시스템 프롬프트",          # 에이전트 역할 정의
    tools=["Read", "Grep", "Glob"],   # 허용 도구 (생략 시 모두 상속)
    model="sonnet",                    # haiku, sonnet, opus, inherit
)
```

## 옵션에 등록

```python
options = ClaudeAgentOptions(
    # Task 도구 필수 (서브에이전트 호출용)
    allowed_tools=["Read", "Edit", "Bash", "Glob", "Grep", "Task"],
    agents={
        "code-reviewer": reviewer_agent,
        "test-runner": test_agent,
        "doc-generator": doc_agent,
    },
)
```

## 호출 방식

### 자동 위임

description을 보고 Claude가 자동으로 적합한 서브에이전트 선택:

```python
# description이 명확하면 자동 위임됨
AgentDefinition(
    description="코드 리뷰 요청 시 사용. 품질, 보안, 성능 분석.",
    ...
)
```

### 명시적 호출

프롬프트에서 직접 지정:

```python
prompt = "code-reviewer 에이전트로 src/ 디렉토리를 리뷰해줘"
```

## 실전 예시

### 코드 리뷰 에이전트

```python
code_reviewer = AgentDefinition(
    description="코드 품질, 보안, 성능을 리뷰합니다. 코드 리뷰 요청 시 사용.",
    prompt="""당신은 시니어 코드 리뷰어입니다.

## 리뷰 관점
1. 코드 품질: 가독성, 유지보수성, SOLID 원칙
2. 보안: 인젝션, XSS, 인증/인가
3. 성능: 불필요한 연산, N+1, 메모리

## 출력 형식
### 요약
[전체 평가]

### 이슈
- [심각도] 파일:라인 - 설명

### 개선 제안
- [우선순위] 제안
""",
    tools=["Read", "Glob", "Grep"],  # 읽기 전용
    model="sonnet",
)
```

### 테스트 실행 에이전트

```python
test_runner = AgentDefinition(
    description="테스트 실행 및 결과 분석. 테스트 요청 시 사용.",
    prompt="""테스트 전문가입니다.

## 작업
1. 테스트 파일 탐색
2. pytest/jest 등 실행
3. 결과 분석

## 출력
- 통과/실패/스킵 수
- 실패 원인 분석
""",
    tools=["Read", "Bash", "Glob", "Grep"],
    model="haiku",  # 빠른 실행
)
```

### 문서화 에이전트

```python
doc_generator = AgentDefinition(
    description="코드에서 문서 생성. 문서화 요청 시 사용.",
    prompt="""기술 문서 작성자입니다.

## 작업
1. 코드 구조 분석
2. 함수/클래스 시그니처 추출
3. 마크다운 문서 생성
""",
    tools=["Read", "Glob", "Grep", "Write"],
    model="sonnet",
)
```

### 보안 감사 에이전트

```python
security_auditor = AgentDefinition(
    description="보안 취약점 감사. 보안 검토 요청 시 사용.",
    prompt="""보안 전문가입니다.

## 검사 항목
- OWASP Top 10
- 하드코딩된 시크릿
- SQL 인젝션
- XSS
- 인증/인가 문제

## 출력
### 발견된 취약점
- [심각도] 파일:라인 - 설명 - 해결방안
""",
    tools=["Read", "Glob", "Grep"],
    model="opus",  # 정밀 분석
)
```

## 병렬 실행

```python
# 메인 에이전트에서 여러 서브에이전트 동시 호출
prompt = """
다음 작업을 병렬로 수행해줘:
1. code-reviewer로 코드 품질 검토
2. security-auditor로 보안 검사
3. test-runner로 테스트 실행
"""

# Claude가 Task 도구를 여러 번 호출
# 최대 10개 동시 실행
```

## 서브에이전트 메시지 감지

```python
async for message in query(prompt=prompt, options=options):
    # 서브에이전트 호출 감지
    if hasattr(message, "content"):
        for block in message.content:
            if getattr(block, "type", None) == "tool_use" and block.name == "Task":
                agent_type = block.input.get("subagent_type")
                print(f"서브에이전트 호출: {agent_type}")

    # 서브에이전트 컨텍스트 내 메시지
    if hasattr(message, "parent_tool_use_id") and message.parent_tool_use_id:
        print("  (서브에이전트 실행 중)")
```

## 제약사항

1. **중첩 불가**: 서브에이전트가 다른 서브에이전트 호출 불가
2. **Task 제외**: 서브에이전트의 tools에 Task 포함하지 않음
3. **최대 10개**: 병렬 실행 시 최대 10개
4. **토큰 증가**: 각 서브에이전트당 ~20K 오버헤드

## description 작성 팁

```yaml
# 나쁜 예
description: 코드 리뷰

# 좋은 예
description: 코드 품질, 보안, 성능을 리뷰합니다. 코드 리뷰 요청 시 사용.
```

- **무엇을 하는지** 명확히
- **언제 사용할지** 트리거 조건
- 구체적 키워드 포함
