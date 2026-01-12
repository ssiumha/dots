# fzf 고급 패턴

## 멀티라인 항목 처리

fzf는 기본적으로 줄 단위로 항목을 구분합니다. 멀티라인 항목은 NUL 구분자를 사용합니다.

### 기본 사용법

```bash
# NUL 구분 입력
find * -print0 | fzf --read0
```

### 표시 옵션

| 옵션 | 설명 |
|------|------|
| `--gap` | 항목 사이 빈 줄 추가 |
| `--highlight-line` | 전체 라인 강조 |
| `--marker-multi-line` | 멀티라인 마커 (╻┃╹) |

### Bash 함수 목록

```bash
declare -f |
  perl -0 -pe 's/^}\n/}\0/gm' |
  bat --plain --language bash --color always |
  fzf --read0 --ansi --layout reverse --multi --highlight-line --gap
```

### ripgrep 결과 청크화

```bash
rg --pretty pattern |
  perl -0 -pe 's/\n\n/\0/gm' |
  fzf --read0 --ansi --multi --highlight-line --layout reverse --gap
```

### 2줄 표시 패턴

```bash
# 경로와 내용을 2줄로
rg --column --line-number --no-heading --color=always pattern |
  perl -pe 's/\n/\0/; s/^([^:]+:){3}/$&\n  /' |
  fzf --read0 --ansi --highlight-line --multi --delimiter : \
      --preview 'bat --style=numbers --color=always --highlight-line {2} {1}'
```

## 프로그래밍 언어 통합

fzf를 외부 프로세스로 호출하여 다양한 언어에서 사용할 수 있습니다.

### 패턴

1. stdin으로 항목 전달
2. fzf 프로세스 실행
3. stdout에서 선택 결과 수신

### Ruby

```ruby
def with_filter(command)
  io = IO.popen(command, 'r+')
  begin
    stdout, $stdout = $stdout, io
    yield rescue nil
  ensure
    $stdout = stdout
  end
  io.close_write
  io.readlines.map(&:chomp)
end

# 사용
result = with_filter('fzf -m') do
  1000.times { |n| puts n; sleep 0.005 }
end
```

### Python

```python
import subprocess
import sys

def with_filter(command, work):
    process = subprocess.Popen(
        command,
        stdin=subprocess.PIPE,
        stdout=subprocess.PIPE,
        text=True,
        shell=True
    )
    original_stdout = sys.stdout
    sys.stdout = process.stdin
    try:
        work()
        process.stdin.close()
    except:
        pass
    finally:
        sys.stdout = original_stdout
    return process.stdout.read().splitlines()

# 사용
def generate_items():
    for i in range(1000):
        print(i)

result = with_filter('fzf -m', generate_items)
```

### Go

```go
package main

import (
    "bufio"
    "fmt"
    "os/exec"
)

func fzfSelect(items []string) (string, error) {
    cmd := exec.Command("fzf", "--height", "40%")
    stdin, _ := cmd.StdinPipe()
    stdout, _ := cmd.StdoutPipe()

    cmd.Start()

    for _, item := range items {
        fmt.Fprintln(stdin, item)
    }
    stdin.Close()

    scanner := bufio.NewScanner(stdout)
    var result string
    if scanner.Scan() {
        result = scanner.Text()
    }

    cmd.Wait()
    return result, nil
}
```

### Node.js

```javascript
const { spawn } = require('child_process');

function fzfSelect(items, options = []) {
  return new Promise((resolve, reject) => {
    const fzf = spawn('fzf', options);

    let result = '';
    fzf.stdout.on('data', (data) => {
      result += data.toString();
    });

    fzf.on('close', (code) => {
      if (code === 0) {
        resolve(result.trim().split('\n'));
      } else {
        resolve([]);
      }
    });

    fzf.stdin.write(items.join('\n'));
    fzf.stdin.end();
  });
}

// 사용
const items = ['apple', 'banana', 'cherry'];
fzfSelect(items, ['--multi']).then(console.log);
```

## 동적 소스 전환

하나의 fzf 인스턴스에서 여러 소스를 전환합니다.

### 파일/디렉토리 전환

```bash
fzf --bind 'ctrl-f:reload:fd --type f' \
    --bind 'ctrl-d:reload:fd --type d' \
    --header 'CTRL-F: files | CTRL-D: directories'
```

### ripgrep ↔ fzf 모드 전환

```bash
RG_PREFIX="rg --column --line-number --no-heading --color=always"

fzf --ansi --disabled \
    --bind "start:reload:$RG_PREFIX {q}" \
    --bind "change:reload:$RG_PREFIX {q} || true" \
    --bind 'ctrl-t:transform:[[ $FZF_PROMPT == "rg> " ]] &&
      echo "unbind(change)+change-prompt(fzf> )+enable-search" ||
      echo "rebind(change)+change-prompt(rg> )+disable-search"' \
    --prompt "rg> "
```

## HTTP 서버 모드

fzf를 서버로 실행하여 원격 제어합니다.

```bash
# 서버 시작
fzf --listen 6266 &

# 원격 조작
curl localhost:6266 -d 'change-query(hello)'
curl localhost:6266 -d 'reload(ls -la)'
curl localhost:6266 -d 'accept'
```

## 상태 저장/복원

```bash
# 쿼리 저장
--bind 'ctrl-s:execute-silent(echo {q} > /tmp/fzf-query)'

# 쿼리 복원
--bind 'ctrl-l:transform-query(cat /tmp/fzf-query)'
```

## 트러블슈팅

| 증상 | 원인 | 해결 |
|------|------|------|
| 느림 | 항목 너무 많음 | `fd` 사용, `--walker-skip` |
| 색상 안 나옴 | ANSI 미파싱 | `--ansi` 추가 |
| 프리뷰 안 뜸 | 명령어 에러 | 터미널에서 직접 테스트 |
| 선택 안 됨 | 필드 문제 | `--nth`, `--delimiter` 확인 |
| 멀티라인 깨짐 | 구분자 문제 | `--read0` + NUL 사용 |
