# 세션 관리

## 세션 ID 캡처

```python
session_id = None

async for message in query(prompt="작업 시작", options=options):
    # init 메시지에서 세션 ID 추출
    if hasattr(message, "subtype") and message.subtype == "init":
        session_id = message.session_id
        print(f"세션 시작: {session_id}")

    if hasattr(message, "result"):
        print(message.result)

# 세션 ID 저장 (재개용)
save_session_id(session_id)
```

## 세션 재개

```python
# 이전 세션 ID로 재개
previous_session = load_session_id()

async for message in query(
    prompt="이전 작업 계속해줘",
    options=ClaudeAgentOptions(
        resume=previous_session,  # 세션 재개
        allowed_tools=["Read", "Edit", "Bash"],
    )
):
    if hasattr(message, "result"):
        print(message.result)
```

## 실전 예시

### 장기 작업 관리

```python
import json
from pathlib import Path

SESSION_FILE = Path(".agent_session.json")

class SessionManager:
    """세션 상태 관리"""

    def __init__(self):
        self.session_id: str | None = None
        self.task_description: str = ""

    def save(self):
        """세션 상태 저장"""
        data = {
            "session_id": self.session_id,
            "task_description": self.task_description,
        }
        SESSION_FILE.write_text(json.dumps(data))

    def load(self) -> bool:
        """세션 상태 로드"""
        if not SESSION_FILE.exists():
            return False

        data = json.loads(SESSION_FILE.read_text())
        self.session_id = data.get("session_id")
        self.task_description = data.get("task_description", "")
        return self.session_id is not None

    def clear(self):
        """세션 초기화"""
        self.session_id = None
        self.task_description = ""
        SESSION_FILE.unlink(missing_ok=True)


async def run_with_session(prompt: str, resume: bool = False):
    """세션 관리가 포함된 에이전트 실행"""
    manager = SessionManager()

    options = ClaudeAgentOptions(
        allowed_tools=["Read", "Edit", "Bash", "Glob", "Grep"],
        permission_mode="acceptEdits",
    )

    # 재개 모드
    if resume and manager.load():
        options.resume = manager.session_id
        print(f"세션 재개: {manager.session_id}")
        print(f"이전 작업: {manager.task_description}")

    async for message in query(prompt=prompt, options=options):
        # 새 세션 ID 캡처
        if hasattr(message, "subtype") and message.subtype == "init":
            manager.session_id = message.session_id
            manager.task_description = prompt
            manager.save()

        if hasattr(message, "result"):
            print(message.result)
            # 완료 시 세션 정리 (선택)
            # manager.clear()
            return message.result

    return None
```

### CLI에서 세션 관리

```python
import argparse

def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("prompt", nargs="?", help="작업 지시")
    parser.add_argument("--resume", action="store_true", help="이전 세션 재개")
    parser.add_argument("--clear", action="store_true", help="세션 초기화")
    args = parser.parse_args()

    manager = SessionManager()

    if args.clear:
        manager.clear()
        print("세션 초기화됨")
        return 0

    if args.resume and not args.prompt:
        if manager.load():
            args.prompt = "이전 작업 계속해줘"
        else:
            print("재개할 세션이 없습니다")
            return 1

    result = asyncio.run(run_with_session(args.prompt, args.resume))
    return 0 if result else 1
```

### 사용법

```bash
# 새 작업 시작
python agent.py "프로젝트 리팩토링해줘"

# 세션 재개 (컨텍스트 유지)
python agent.py --resume "계속해줘"

# 특정 작업으로 재개
python agent.py --resume "테스트도 추가해줘"

# 세션 초기화
python agent.py --clear
```

## 세션 활용 시나리오

### 1. 대화 맥락 유지

```python
# 1차 쿼리
"src/ 디렉토리 구조 분석해줘"
# Claude가 파일 구조 파악

# 2차 쿼리 (세션 재개)
"방금 분석한 구조에서 utils/ 부분만 리팩토링해줘"
# 이전 분석 결과를 기억하고 있음
```

### 2. 중단된 작업 재개

```python
# 작업 중 중단됨
"100개 파일 마이그레이션해줘"
# 50개 완료 후 중단

# 재개
"계속해줘"
# 51번째부터 재개
```

### 3. 점진적 개선

```python
# 1단계
"기본 API 엔드포인트 만들어줘"

# 2단계 (세션 유지)
"방금 만든 API에 인증 추가해줘"

# 3단계 (세션 유지)
"테스트 코드도 작성해줘"
```

## 주의사항

1. **세션 유효성**: 세션 ID는 일정 시간 후 만료될 수 있음
2. **컨텍스트 크기**: 긴 세션은 컨텍스트 한도에 도달할 수 있음
3. **세션 저장**: 프로세스 종료 시 세션 ID를 파일로 저장 권장
4. **보안**: 세션 ID는 민감 정보처럼 관리
