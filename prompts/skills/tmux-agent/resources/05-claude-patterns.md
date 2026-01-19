# Claude 인스턴스 통신 가이드

## Claude 프로세스 식별

### pane에서 Claude 찾기

```bash
# Claude 실행 중인 모든 pane
tmux list-panes -a -F "#{session_name}:#{window_index}.#{pane_index} [#{pane_current_command}]" \
    -f "#{m:*claude*,#{pane_current_command}}"

# 현재 세션에서만
tmux list-panes -F "#{window_index}.#{pane_index}" \
    -f "#{m:*claude*,#{pane_current_command}}"
```

### Claude 상태 확인

```bash
# pane 출력의 마지막 줄로 상태 판단
tmux capture-pane -t {target} -p | tail -3
```

**상태 패턴:**
- `You:` 또는 `> ` → 입력 대기 (IDLE)
- `Claude is thinking...` → 처리 중 (ACTIVE)
- 스피너 (`⠋⠙⠹...`) → 처리 중 (ACTIVE)
- `[Y/n]` → 권한 요청 대기

---

## 프롬프트 전송

### 기본 전송

```bash
TARGET="1.0"
PROMPT="간단한 질문입니다"

# 1. Idle 확인
# 2. 프롬프트 전송
tmux send-keys -t "$TARGET" "$PROMPT" && sleep 0.5 && tmux send-keys -t "$TARGET" Enter
```

### 긴 프롬프트 (다중 라인)

```bash
# 방법 1: 임시 파일 사용 (권장)
cat > /tmp/prompt.txt << 'EOF'
다음 코드를 리뷰해주세요:

```python
def hello():
    print("world")
```

특히 성능과 가독성 관점에서 피드백 부탁드립니다.
EOF

# 파일 내용을 클립보드로 (macOS)
cat /tmp/prompt.txt | pbcopy
tmux send-keys -t "$TARGET" "$(pbpaste)" && sleep 1 && tmux send-keys -t "$TARGET" Enter

# 방법 2: 라인별 전송 (주의: 느림)
tmux send-keys -t "$TARGET" "다음 코드를 리뷰해주세요:" && sleep 0.3 && tmux send-keys -t "$TARGET" Enter
tmux send-keys -t "$TARGET" "" && sleep 0.3 && tmux send-keys -t "$TARGET" Enter  # 빈 줄
tmux send-keys -t "$TARGET" "\`\`\`python" && sleep 0.3 && tmux send-keys -t "$TARGET" Enter
# ...
```

### 특수문자 포함 프롬프트

```bash
# 코드 블록 전송 시 백틱 이스케이프
tmux send-keys -t "$TARGET" '```python' && sleep 0.3 && tmux send-keys -t "$TARGET" Enter
tmux send-keys -t "$TARGET" 'print("hello")' && sleep 0.3 && tmux send-keys -t "$TARGET" Enter
tmux send-keys -t "$TARGET" '```' && sleep 0.3 && tmux send-keys -t "$TARGET" Enter
```

---

## 응답 대기

### Polling 방식

```bash
wait_for_claude_response() {
    local target="$1"
    local timeout="${2:-60}"
    local interval=2
    local elapsed=0

    while [ $elapsed -lt $timeout ]; do
        local output
        output=$(tmux capture-pane -t "$target" -p | tail -5)

        # 입력 대기 상태 (응답 완료)
        if echo "$output" | grep -qE '(You:|^> |^\$ )$'; then
            echo "응답 완료"
            return 0
        fi

        # 권한 요청 감지
        if echo "$output" | grep -qE '\[Y/n\]|\(y/N\)'; then
            echo "권한 요청 대기 중"
            return 2
        fi

        sleep $interval
        elapsed=$((elapsed + interval))
    done

    echo "타임아웃"
    return 1
}
```

### 응답 추출

```bash
extract_claude_response() {
    local target="$1"
    local lines="${2:-100}"

    # 마지막 N줄 캡처
    local output
    output=$(tmux capture-pane -t "$target" -p -S "-$lines")

    # Claude 응답 부분만 추출 (간단한 방식)
    # 실제로는 더 정교한 파싱 필요
    echo "$output"
}
```

---

## 권한 요청 처리

Claude가 파일 수정, 명령 실행 등 권한을 요청할 때:

```bash
handle_permission_request() {
    local target="$1"
    local response="${2:-y}"  # y 또는 n

    # 권한 요청 확인
    local output
    output=$(tmux capture-pane -t "$target" -p | tail -3)

    if echo "$output" | grep -qE '\[Y/n\]|\(y/N\)'; then
        tmux send-keys -t "$target" "$response" && sleep 0.3 && tmux send-keys -t "$target" Enter
        return 0
    fi

    return 1  # 권한 요청 없음
}
```

### 자동 승인 (주의 필요)

```bash
auto_approve_loop() {
    local target="$1"
    local timeout=300  # 5분
    local elapsed=0

    while [ $elapsed -lt $timeout ]; do
        if handle_permission_request "$target" "y"; then
            echo "권한 승인됨"
        fi

        sleep 2
        elapsed=$((elapsed + 2))

        # 완료 확인
        local output
        output=$(tmux capture-pane -t "$target" -p | tail -3)
        if echo "$output" | grep -qE 'You:$'; then
            echo "작업 완료"
            return 0
        fi
    done
}
```

---

## 통신 프로토콜

### 단방향 통신 (요청 → 응답)

```bash
ask_claude() {
    local target="$1"
    local prompt="$2"
    local timeout="${3:-60}"

    # 1. Idle 확인
    local status
    status=$(check_idle "$target")
    if [ "$status" != "IDLE" ]; then
        echo "Error: Claude가 준비되지 않음 ($status)"
        return 1
    fi

    # 2. 프롬프트 전송
    tmux send-keys -t "$target" "$prompt" && sleep 0.5 && tmux send-keys -t "$target" Enter

    # 3. 응답 대기
    wait_for_claude_response "$target" "$timeout"

    # 4. 응답 추출
    extract_claude_response "$target"
}
```

### 양방향 통신 (Claude ↔ Claude)

```bash
claude_to_claude() {
    local from="$1"    # 질문하는 Claude
    local to="$2"      # 응답하는 Claude
    local prompt="$3"

    # 1. 대상 Claude에 질문 전송
    ask_claude "$to" "$prompt" 120

    # 2. 응답 캡처
    local response
    response=$(extract_claude_response "$to" 200)

    # 3. 원래 Claude에 응답 전달
    ask_claude "$from" "다른 Claude의 응답: $response"
}
```

---

## 에러 처리

### 타임아웃

```bash
if ! wait_for_claude_response "$target" 60; then
    echo "Claude 응답 타임아웃"
    # 선택: 취소 또는 재시도
    tmux send-keys -t "$target" C-c  # 취소
fi
```

### 에러 감지

```bash
check_claude_error() {
    local target="$1"
    local output
    output=$(tmux capture-pane -t "$target" -p | tail -20)

    if echo "$output" | grep -qE "(Error|error|failed|Failed)"; then
        echo "에러 감지됨"
        echo "$output" | grep -iE "(error|failed)" | head -5
        return 1
    fi

    return 0
}
```

---

## 병렬 작업 오케스트레이션

```bash
parallel_claude_tasks() {
    local targets=("$@")  # pane 타겟 목록
    local prompts=()      # 각 pane에 보낼 프롬프트

    # 1. 모든 pane에 프롬프트 전송 (병렬)
    for i in "${!targets[@]}"; do
        tmux send-keys -t "${targets[$i]}" "${prompts[$i]}" && \
        sleep 0.3 && \
        tmux send-keys -t "${targets[$i]}" Enter &
    done
    wait

    # 2. 모든 pane 완료 대기
    local all_done=false
    local timeout=300
    local elapsed=0

    while [ "$all_done" = false ] && [ $elapsed -lt $timeout ]; do
        all_done=true
        for target in "${targets[@]}"; do
            local status
            status=$(check_idle "$target")
            if [ "$status" != "IDLE" ]; then
                all_done=false
                break
            fi
        done
        sleep 5
        elapsed=$((elapsed + 5))
    done

    # 3. 결과 수집
    for target in "${targets[@]}"; do
        echo "=== $target ==="
        extract_claude_response "$target" 50
        echo ""
    done
}
```
