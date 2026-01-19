# Idle 상태 감지 패턴

## 감지 원리

1. **출력 변화 추적**: 2초 간격으로 pane 출력 비교
2. **프롬프트 패턴 매칭**: 명령 대기 상태 확인
3. **상태 판정**: IDLE / ACTIVE / FAILED

---

## Shell 프롬프트 패턴

### Bash

```regex
# 기본 프롬프트
\$\s*$
user@host:~\$\s*$

# PS1 커스텀
\]\$\s*$
```

### Zsh

```regex
# 기본
%\s*$

# Oh-My-Zsh
❯\s*$
➜\s*$

# Powerlevel10k
╰─\s*$
```

### Fish

```regex
>\s*$
⋊>\s*$
```

### 통합 패턴 (권장)

```bash
# 대부분의 shell 프롬프트 감지
grep -qE '(\$|%|❯|➜|>)\s*$'
```

---

## REPL 프롬프트 패턴

### Python

```regex
>>>\s*$
\.\.\.\s*$    # 다중 라인 입력 중
In \[\d+\]:\s*$    # IPython
```

### Node.js

```regex
>\s*$
\.\.\.\s*$
```

### Ruby (irb/pry)

```regex
irb\(.*\):\d+:\d+>\s*$
\[\d+\] pry\(.*\)>\s*$
```

### 통합 패턴

```bash
grep -qE '(>>>|In \[\d+\]:|\[\d+\] pry|irb.*>|>)\s*$'
```

---

## Claude Code 패턴

### 입력 대기

```regex
# Claude Code 프롬프트
You:\s*$
>\s*$

# 권한 요청 대기
\[Y/n\]\s*$
\(y/N\)\s*$
```

### 작업 중

```regex
# 스피너/진행 표시
⠋|⠙|⠹|⠸|⠼|⠴|⠦|⠧|⠇|⠏
\.{3,}$
```

### 완료

```regex
# 작업 완료 후 프롬프트 복귀
You:\s*$
```

---

## 에러 패턴

### 공통 에러

```regex
Error:
error:
ERROR
FAIL
Failed
Traceback
panic:
Exception
```

### 언어별 에러

```bash
# Python
grep -E "Traceback|SyntaxError|NameError|TypeError"

# Node.js
grep -E "Error:|TypeError:|ReferenceError:"

# Rust
grep -E "error\[E\d+\]:|panicked at"

# Go
grep -E "panic:|fatal error:"
```

---

## 완료 패턴

```regex
# 성공
✓
Done
Success
Passed
Complete

# 테스트 결과
\d+ passed
All tests passed
```

---

## 상태 판정 로직

```bash
#!/bin/bash
TARGET="$1"
TIMEOUT=30
INTERVAL=2

check_idle() {
    local output
    output=$(tmux capture-pane -t "$TARGET" -p | tail -5)

    # 에러 패턴 확인
    if echo "$output" | grep -qE "(Error:|FAIL|Traceback|panic:)"; then
        echo "FAILED"
        return
    fi

    # 프롬프트 패턴 확인 (Shell + REPL + Claude)
    if echo "$output" | grep -qE '(\$|%|❯|>>>|You:)\s*$'; then
        echo "IDLE"
        return
    fi

    echo "ACTIVE"
}

wait_for_idle() {
    local elapsed=0

    while [ $elapsed -lt $TIMEOUT ]; do
        local status
        status=$(check_idle)

        case "$status" in
            "IDLE")
                echo "완료: Idle 상태"
                return 0
                ;;
            "FAILED")
                echo "에러 발생"
                return 1
                ;;
            "ACTIVE")
                sleep $INTERVAL
                elapsed=$((elapsed + INTERVAL))
                ;;
        esac
    done

    echo "타임아웃: $TIMEOUT초 초과"
    return 2
}
```

---

## 고급: 2회 캡처 비교

단순 프롬프트 매칭보다 정확한 방법:

```bash
detect_idle_by_diff() {
    local target="$1"

    # 1차 캡처
    local output1
    output1=$(tmux capture-pane -t "$target" -p | tail -10)

    sleep 2

    # 2차 캡처
    local output2
    output2=$(tmux capture-pane -t "$target" -p | tail -10)

    # 비교
    if [ "$output1" = "$output2" ]; then
        # 변화 없음 → 프롬프트 확인
        if echo "$output2" | grep -qE '(\$|%|❯|>>>|You:)\s*$'; then
            echo "IDLE"
        else
            echo "WAITING"  # 출력 중단됨, 프롬프트 아님
        fi
    else
        echo "ACTIVE"
    fi
}
```

---

## 주의사항

1. **스피너/애니메이션**: 출력 변화 있지만 실제론 대기 중일 수 있음
   - 해결: 2초 이상 간격, 3회 확인

2. **긴 출력 중단**: 출력이 멈췄지만 프롬프트 아닐 수 있음
   - 해결: 프롬프트 패턴 반드시 확인

3. **커스텀 프롬프트**: PS1이 특이하면 감지 실패
   - 해결: 사용자에게 프롬프트 패턴 질문

4. **비대화형 명령**: `tail -f` 등은 영원히 ACTIVE
   - 해결: 타임아웃 설정 필수
