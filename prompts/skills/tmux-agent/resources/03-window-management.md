# 윈도우/Pane 관리 가이드

## Naming Convention

### 윈도우 이름

| 패턴 | 용도 | 예시 |
|------|------|------|
| `agent-{N}` | Claude 인스턴스 | `agent-1`, `agent-2` |
| `monitor-{purpose}` | 모니터링 전용 | `monitor-logs`, `monitor-build` |
| `build-{project}` | 빌드 프로세스 | `build-frontend` |
| `test-{scope}` | 테스트 실행 | `test-unit`, `test-e2e` |
| `dev-{service}` | 개발 서버 | `dev-api`, `dev-web` |

### Pane 타이틀

```bash
# Pane 타이틀 설정
tmux select-pane -t {target} -T "title"

# 권장 형식
tmux select-pane -t 0.0 -T "claude-main"
tmux select-pane -t 0.1 -T "test-runner"
```

---

## 레이아웃 패턴

### 기본: 2-pane (좌우 분할)

```bash
# Claude 왼쪽, 작업 오른쪽
tmux new-window -n "work"
tmux split-window -h -t 0

# 결과:
# +----------+----------+
# |  Claude  |   Work   |
# |   (0.0)  |   (0.1)  |
# +----------+----------+
```

### 모니터링: 3-pane

```bash
# Claude 왼쪽, 로그/빌드 오른쪽
tmux new-window -n "monitor"
tmux split-window -h -t 0 -p 40
tmux split-window -v -t 1

# 결과:
# +----------+----------+
# |          |   Logs   |
# |  Claude  |   (0.1)  |
# |   (0.0)  +----------+
# |          |  Build   |
# |          |   (0.2)  |
# +----------+----------+
```

### 병렬 작업: 4-pane

```bash
# Claude Company 스타일 (1 매니저 + 3 워커)
tmux new-window -n "parallel"
tmux split-window -h -t 0 -p 75
tmux split-window -v -t 1
tmux split-window -v -t 1

# 결과:
# +--------+--------+
# |        | Agent1 |
# |Manager +--------+
# | (0.0)  | Agent2 |
# |        +--------+
# |        | Agent3 |
# +--------+--------+
```

### 대시보드: 5-pane

```bash
tmux new-window -n "dashboard"
tmux split-window -h -t 0 -p 70
tmux split-window -v -t 0 -p 70
tmux split-window -v -t 2
tmux split-window -v -t 2

# 결과:
# +----------+--------+
# |          | Pane 2 |
# |  Main    +--------+
# |  (0.0)   | Pane 3 |
# +----------+--------+
# | Status   | Pane 4 |
# |  (0.1)   |        |
# +----------+--------+
```

---

## 윈도우 생성

### 기본 생성

```bash
# 빈 윈도우
tmux new-window -n "name"

# 명령 실행하며 생성
tmux new-window -n "claude" "claude"

# 특정 디렉토리에서
tmux new-window -n "project" -c "/path/to/project"
```

### 백그라운드 생성

```bash
# 현재 윈도우 유지하며 생성
tmux new-window -d -n "background"
```

---

## Pane 분할

### 분할 옵션

```bash
# 수평 분할 (좌우)
tmux split-window -h

# 수직 분할 (상하)
tmux split-window -v

# 비율 지정 (새 pane이 40% 차지)
tmux split-window -h -p 40

# 특정 pane 기준으로 분할
tmux split-window -h -t 0.1

# 명령 실행하며 분할
tmux split-window -h "htop"
```

### 레이아웃 프리셋

```bash
# 균등 수평
tmux select-layout even-horizontal

# 균등 수직
tmux select-layout even-vertical

# 메인 수평 (첫 pane 크게)
tmux select-layout main-horizontal

# 메인 수직
tmux select-layout main-vertical

# 타일
tmux select-layout tiled
```

---

## Pane 조작

### 크기 조절

```bash
# 현재 pane 크기 조절
tmux resize-pane -L 10    # 왼쪽으로 10칸
tmux resize-pane -R 10    # 오른쪽으로 10칸
tmux resize-pane -U 5     # 위로 5칸
tmux resize-pane -D 5     # 아래로 5칸

# 특정 크기로
tmux resize-pane -x 80    # 너비 80
tmux resize-pane -y 20    # 높이 20
```

### 포커스 이동

```bash
# 인덱스로 선택
tmux select-pane -t 0.1

# 방향으로 이동
tmux select-pane -L    # 왼쪽
tmux select-pane -R    # 오른쪽
tmux select-pane -U    # 위
tmux select-pane -D    # 아래
```

### Pane 종료

```bash
# 특정 pane 종료
tmux kill-pane -t 0.1

# 다른 pane 모두 종료 (현재만 유지)
tmux kill-pane -a
```

---

## 세션 관리

### 세션 생성

```bash
# 새 세션
tmux new-session -s "project-name"

# 분리 상태로 생성
tmux new-session -d -s "background"
```

### 세션 연결

```bash
# 세션에 연결
tmux attach-session -t "project-name"

# 줄여서
tmux attach -t "project-name"
tmux a -t "project-name"
```

### 세션 목록

```bash
tmux list-sessions
tmux ls
```

---

## 자동화 스크립트

### Claude 작업 환경 초기화

```bash
#!/bin/bash
SESSION="claude-work"

# 세션 생성
tmux new-session -d -s "$SESSION" -n "main"

# Claude 윈도우 설정
tmux send-keys -t "$SESSION:main" "claude" Enter

# 모니터 윈도우 추가
tmux new-window -t "$SESSION" -n "monitor"
tmux split-window -h -t "$SESSION:monitor"
tmux send-keys -t "$SESSION:monitor.0" "tail -f logs/app.log" Enter
tmux send-keys -t "$SESSION:monitor.1" "htop" Enter

# 테스트 윈도우 추가
tmux new-window -t "$SESSION" -n "test"

# 첫 윈도우로 돌아가기
tmux select-window -t "$SESSION:main"

# 연결
tmux attach -t "$SESSION"
```

### 병렬 Claude 환경

```bash
#!/bin/bash
SESSION="claude-parallel"
AGENTS=3

tmux new-session -d -s "$SESSION" -n "manager"
tmux send-keys -t "$SESSION:manager" "claude" Enter

for i in $(seq 1 $AGENTS); do
    tmux new-window -t "$SESSION" -n "agent-$i"
    tmux send-keys -t "$SESSION:agent-$i" "claude" Enter
done

tmux select-window -t "$SESSION:manager"
tmux attach -t "$SESSION"
```
