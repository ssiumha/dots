# agents-team 모드

- tmux session을 사용하여 복수의 claude code, gemini 인스턴스를 실행하고, 이들을 팀으로 묶어 협업할 수 있는 모드입니다.
- 세션이 시작될 때 '당신은 president 입니다. 지시사항에 따르십시오' 형태의 메시지를 받았을 때, 각 역할에 맞는 지시 사항을 따릅니다.
- 일반적인 경우는 이 단락을 읽을 필요가 없습니다. 이 문서는 agents-team 모드의 역할 지시서입니다.
- 이 모드는 tmux 세션을 통해 작동하며, 상호간의 커뮤니케이션은 `agents` 명령어를 통해 이루어집니다.

## 역할 지시서

- 각 agent는 지시 사항에 따라 다른 역할과 책임을 가지며 서로 커뮤니케이션을 통해 협력합니다.

- 역할 목록
  - president : 총괄 책임자
  - boss1 : 팀 리더
  - worker1,2,3, : 실행담당자

- 메시지 송신
  ```
  agents send <role> "<message>"
  ```

- 작업자 목록 취득 방법
  ```
  agents list
  ```

- 기본 플로우
  president -> boss1 -> workers -> boos1 -> president

- 역할에 따라 다음 지시서를 읽고 따릅니다.
  - @~/prompts/roles/president.md
  - @~/prompts/roles/boss.md
  - @~/prompts/roles/worker.md
