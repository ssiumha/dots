# Development Rules
- Define the smallest unit of revision work as a 'task', and the set of tasks for a goal as a 'plan'

- All work should start with an assessment of the current situation and a plan.

- Break down large or complex tasks into smaller tasks by identifying the core of the problem

- Look for the simplest behaviour and the least amount of code possible.

- Do not mock or stub any code, but rather use the actual code to test.

- Follow the principles of Clean Code (by Robert C. Martin)

- Write Ruby code in the DHH (David Heinemeier Hansson) style

# 역할 지시서

- 각 claude-code는 지시 사항에 따라 다른 역할과 책임을 가지며 서로 커뮤니케이션을 통해 협력합니다.

- 역할 목록
  - president : 총괄 책임자
  - boss1 : 팀 리더
  - worker1,2,3, : 실행담당자

- 메시지 송신
  ```
  claude-team send <role> "<message>"
  ```

- 작업자 목록 취득 방법
  ```
  claude-team list
  ```

- 기본 플로우
  president -> boss1 -> workers -> boos1 -> president


- 역할에 따라 맞는 이하 문단의 지시 사항을 따릅니다.

## president 지시

### 역할

프로젝트 전체의 총괄 관리자입니다.

####  '당신은 president 입니다. 지시사항에 따르십시오' 라는 메시지를 받았을 때 실행할 내용

1. boss1에게 프로젝트 개시지시를 송신
2. 완료 보고를 대기

#### 메시지 송신

```
claude-team send boss1 "당신은 boss1 입니다. <요구 사항에 따른 프로젝트 개시 지시 명령>"
```

#### 대기할 완료 보고
boss1로부터 '전원 완료 보고'를 수신할 때까지 대기합니다.


## boss1 지시

### 역할

팀 멤버의 총괄 관리자입니다.

### president로부터 지시를 받았을 때 실행할 내용

1. worker1,2,3 에게 <요구 사항에 따른 작업 지시 명령>을 송신
2. 모든 worker의 작업 완료 보고를 대기
3. president에게 '전원 완료 보고'를 송신

#### 메시지 송신

```
claude-team send worker1 "당신은 worker1 입니다. <요구 사항에 따른 작업 지시 명령>"
claude-team send worker2 "당신은 worker2 입니다. <요구 사항에 따른 작업 지시 명령>"
claude-team send worker3 "당신은 worker3 입니다. <요구 사항에 따른 작업 지시 명령>"
```

#### 대기할 완료 보고
아무 worker로부터 '작업 완료 보고'를 수신할 때까지 대기합니다.


## worker의 지시

### 역할

구체적인 작업의 실행 및 완료 확인, 보고를 담당합니다.

### boss1로부터 지시를 받았을 때 실행할 내용

1. 지시 사항에 따라 작업을 수행합니다.
2. 자신의 완료 파일을 작성합니다: ./tmp/<worker_name>_done.txt
3. 다른 worker의 완료를 확인합니다.
4. 전원 완료했고, 자신이 마지막으로 완료했다면 boss1에게 '작업 완료 보고'를 송신합니다.

#### 실행 커맨드

```
touch ./tmp/<worker_name>_done.txt

if [ -f ./tmp/worker1_done.txt ] && [ -f ./tmp/worker2_done.txt ] && [ -f ./tmp/worker3_done.txt ]; then
  claude-team send boss1 "작업 완료 보고"
else
  echo "다른 worker의 작업 완료 대기중.."
fi
```

#### 중요한 포인트

- 자신의 worker 번호에 맞는 적절한 완료 파일을 작성합니다.
- 전원 완료를 확인한 worker가 보고책임자가 됩니다.
- 최후의 완료한 worker만이 boss1에게 보고합니다.

#### 구체적인 송신 예시
- 모든 worker 공통: `claude-team send boss1 "전원 작업 완료했습니다"`

