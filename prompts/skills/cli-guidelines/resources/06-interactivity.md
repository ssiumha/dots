# 상호작용성

## TTY 감지

프롬프트는 대화형 터미널에서만 표시합니다:

```python
import sys

def is_interactive():
    return sys.stdin.isatty() and sys.stdout.isatty()

def prompt_user(message, default=None):
    if not is_interactive():
        if default is not None:
            return default
        raise RuntimeError(f"No TTY available for prompt: {message}")

    if default:
        response = input(f"{message} [{default}]: ")
        return response or default
    return input(f"{message}: ")
```

## --no-input 플래그

모든 상호작용을 비활성화하는 옵션을 제공합니다:

```bash
# 대화형 (기본)
$ myapp install
Package not found. Search online? [Y/n]

# 비대화형
$ myapp install --no-input
Error: Package not found
```

### 구현

```python
def should_prompt(args):
    if args.no_input:
        return False
    if not sys.stdin.isatty():
        return False
    return True
```

## 프롬프트 유형

### 예/아니오

```bash
Continue? [Y/n]    # Y가 기본
Continue? [y/N]    # N이 기본
```

```python
def confirm(message, default=True):
    suffix = "[Y/n]" if default else "[y/N]"
    response = input(f"{message} {suffix} ").lower()

    if not response:
        return default
    return response in ('y', 'yes')
```

### 선택

```bash
Select environment:
  1) production
  2) staging
  3) development
Choice [1]:
```

```python
def select(message, options, default=0):
    print(message)
    for i, opt in enumerate(options, 1):
        print(f"  {i}) {opt}")

    response = input(f"Choice [{default + 1}]: ")
    if not response:
        return options[default]

    idx = int(response) - 1
    return options[idx]
```

### 텍스트 입력

```bash
Project name: my-project
Description []: A cool project
```

### 비밀번호 입력

```python
import getpass

password = getpass.getpass("Password: ")  # 에코 비활성화
```

## 탈출 수단

사용자가 언제든 종료할 수 있어야 합니다:

```bash
# Ctrl-C로 즉시 종료
$ myapp setup
Project name: ^C
Aborted.
```

### Python 구현

```python
import signal
import sys

def handle_interrupt(signum, frame):
    print("\nAborted.", file=sys.stderr)
    sys.exit(130)  # 128 + SIGINT(2)

signal.signal(signal.SIGINT, handle_interrupt)
```

**vim처럼 하지 말 것**: 종료 방법이 불명확한 인터페이스 피하기

## 진행 중 취소

장시간 작업에서 취소 가능하게:

```bash
$ myapp download
Downloading... [████████░░░░] 50%
^C
Download cancelled. Partial file removed.
```

```python
import signal

cancelled = False

def handle_cancel(signum, frame):
    global cancelled
    cancelled = True
    print("\nCancelling...", file=sys.stderr)

signal.signal(signal.SIGINT, handle_cancel)

for chunk in download():
    if cancelled:
        cleanup()
        sys.exit(130)
    process(chunk)
```

## 편집기 호출

긴 텍스트 입력에 편집기 사용:

```bash
$ myapp commit
# 편집기 열림 (vim, nano 등)
```

```python
import os
import subprocess
import tempfile

def edit_text(initial=""):
    editor = os.environ.get('EDITOR', 'vim')

    with tempfile.NamedTemporaryFile(mode='w', suffix='.txt', delete=False) as f:
        f.write(initial)
        temp_path = f.name

    try:
        subprocess.run([editor, temp_path], check=True)
        with open(temp_path) as f:
            return f.read()
    finally:
        os.unlink(temp_path)
```

## 자동완성

탭 자동완성을 지원합니다:

### Bash 완성 스크립트

```bash
# myapp-completion.bash
_myapp_completions() {
    local cur="${COMP_WORDS[COMP_CWORD]}"
    COMPREPLY=($(compgen -W "init build deploy --help --version" -- "$cur"))
}
complete -F _myapp_completions myapp
```

### CLI 라이브러리 지원

- Click: `click-completion`
- argparse: `argcomplete`
- Cobra (Go): 내장 지원
- clap (Rust): `clap_complete`

## 히스토리 저장

대화형 셸에서 입력 히스토리를 저장:

```python
import readline
import os

history_file = os.path.expanduser("~/.myapp_history")

try:
    readline.read_history_file(history_file)
except FileNotFoundError:
    pass

import atexit
atexit.register(readline.write_history_file, history_file)
```

## REPL 모드

대화형 셸 제공:

```bash
$ myapp shell
myapp> list
file1.txt
file2.txt
myapp> exit
```

```python
import cmd

class MyAppShell(cmd.Cmd):
    intro = "Welcome to myapp shell. Type 'help' for commands."
    prompt = "myapp> "

    def do_list(self, arg):
        """List files"""
        for f in list_files():
            print(f)

    def do_exit(self, arg):
        """Exit shell"""
        return True

if __name__ == '__main__':
    MyAppShell().cmdloop()
```

## 마법사/위저드

복잡한 설정을 단계별로 안내:

```bash
$ myapp init

Welcome to myapp setup!

Step 1/3: Project Configuration
Project name: my-project
Description []: A cool project

Step 2/3: Database Configuration
Database type (postgres/mysql/sqlite) [postgres]:
Host [localhost]:
Port [5432]:

Step 3/3: Confirmation
Review your settings:
  Project: my-project
  Database: postgres://localhost:5432

Create project? [Y/n]

Project created! Next steps:
  cd my-project
  myapp run
```
