# 출력 캡처 패턴 가이드

## capture-pane 옵션

```bash
# 기본 (현재 화면만)
tmux capture-pane -t {target} -p

# 히스토리 포함
tmux capture-pane -t {target} -p -S -100    # 최근 100줄
tmux capture-pane -t {target} -p -S -       # 전체 히스토리

# ANSI 이스케이프 코드 포함
tmux capture-pane -t {target} -p -e

# 특정 범위
tmux capture-pane -t {target} -p -S -50 -E -1    # 마지막 50줄
```

---

## 에러 패턴

### 공통 에러 키워드

```bash
# grep 패턴
grep -iE "(error|fail|exception|panic|fatal)"
```

### 언어별 에러 패턴

#### Python

```regex
Traceback \(most recent call last\):
  File ".*", line \d+
.*Error:.*
SyntaxError:
NameError:
TypeError:
ValueError:
ImportError:
ModuleNotFoundError:
```

```bash
grep -E "Traceback|Error:|Exception"
```

#### Node.js / JavaScript

```regex
Error:.*
TypeError:.*
ReferenceError:.*
SyntaxError:.*
    at .*:\d+:\d+
```

```bash
grep -E "Error:|at .+:\d+:\d+"
```

#### Rust

```regex
error\[E\d+\]:.*
   --> .*:\d+:\d+
thread '.*' panicked at
```

```bash
grep -E "error\[E|panicked at"
```

#### Go

```regex
panic:.*
fatal error:.*
.*\.go:\d+:.*
```

```bash
grep -E "panic:|fatal error:|\.go:\d+:"
```

#### Shell (Bash/Zsh)

```regex
.*: command not found
.*: No such file or directory
.*: Permission denied
```

```bash
grep -E "command not found|No such file|Permission denied"
```

---

## 완료 패턴

### 성공 키워드

```bash
grep -iE "(success|done|complete|passed|✓|✔)"
```

### 테스트 결과

```regex
# Jest
Tests:.*\d+ passed
Test Suites:.*passed

# pytest
=+ \d+ passed.*=+
PASSED

# Go test
ok\s+.*\d+\.\d+s
PASS

# Rust (cargo test)
test result: ok\. \d+ passed
```

### 빌드 결과

```regex
# npm/yarn
added \d+ packages
Done in \d+\.\d+s

# cargo
Compiling.*
Finished .* target

# make
make\[\d+\]: Leaving directory
```

---

## 진행 중 패턴

### 스피너/애니메이션

```regex
[⠋⠙⠹⠸⠼⠴⠦⠧⠇⠏]
[|/-\\]
\.{3,}
```

### 진행 표시

```regex
\[\s*\d+%\s*\]
\d+/\d+
Processing.*
Loading.*
Downloading.*
Installing.*
```

---

## 프롬프트 패턴

### Shell

```regex
# Bash
.*\$\s*$
.*#\s*$    # root

# Zsh
.*%\s*$
.*❯\s*$
.*➜\s*$

# Fish
.*>\s*$
```

### REPL

```regex
# Python
>>>\s*$
\.\.\.\s*$
In \[\d+\]:\s*$

# Node
>\s*$

# Ruby
irb.*>\s*$
pry.*>\s*$
```

### Claude Code

```regex
You:\s*$
>\s*$
```

---

## 실용 스크립트

### 상태 판정 함수

```bash
analyze_output() {
    local output="$1"

    # 에러 확인
    if echo "$output" | grep -qiE "(error|fail|panic|exception)"; then
        echo "ERROR"
        return 1
    fi

    # 완료 확인
    if echo "$output" | grep -qiE "(success|done|passed|complete|✓)"; then
        echo "SUCCESS"
        return 0
    fi

    # 진행 중 확인
    if echo "$output" | grep -qE "(\d+%|Processing|Loading|Installing)"; then
        echo "RUNNING"
        return 0
    fi

    # 프롬프트 확인
    if echo "$output" | grep -qE '(\$|%|❯|>>>|>)\s*$'; then
        echo "IDLE"
        return 0
    fi

    echo "UNKNOWN"
    return 0
}
```

### 에러 추출 함수

```bash
extract_errors() {
    local target="$1"
    local lines="${2:-50}"

    tmux capture-pane -t "$target" -p -S "-$lines" | \
        grep -iE "(error|fail|exception|panic)" | \
        head -10
}
```

### 마지막 명령 결과 확인

```bash
check_last_command() {
    local target="$1"

    # 마지막 20줄 캡처
    local output
    output=$(tmux capture-pane -t "$target" -p | tail -20)

    # 분석
    analyze_output "$output"
}
```

---

## 패턴 매칭 팁

1. **대소문자**: `-i` 옵션으로 대소문자 무시
2. **컨텍스트**: `-A`, `-B` 옵션으로 전후 라인 포함
3. **ANSI 코드**: 캡처 시 `-e` 옵션 사용 여부 결정
4. **멀티라인**: `grep -z` 또는 `awk` 사용

```bash
# 에러와 그 다음 3줄
tmux capture-pane -t 1.0 -p | grep -A 3 "Error:"

# ANSI 코드 제거 후 검색
tmux capture-pane -t 1.0 -p | sed 's/\x1b\[[0-9;]*m//g' | grep "Error"
```
