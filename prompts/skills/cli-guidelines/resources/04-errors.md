# 오류 처리

## 종료 코드

```
0    성공
1    일반 오류
2    잘못된 사용법 (인자 오류)
126  권한 오류
127  명령을 찾을 수 없음
128+ 시그널로 인한 종료
```

### Python 예시

```python
import sys

# 성공
sys.exit(0)

# 일반 오류
sys.exit(1)

# 사용법 오류
if not args.input:
    print("Error: input file required", file=sys.stderr)
    sys.exit(2)
```

## 인간 친화적 오류 메시지

### 원칙

1. **무슨 일이 일어났는지** 설명
2. **왜 일어났는지** 설명 (가능하면)
3. **어떻게 해결하는지** 제안

### 예시

```bash
# 나쁨
Error: ENOENT

# 좋음
Can't read config.yaml: file not found.
Create it with: myapp init

# 나쁨
Error: Permission denied

# 좋음
Can't write to /etc/myapp.conf
This file is owned by root. Try: sudo myapp config

# 나쁨
Error: Connection refused

# 좋음
Can't connect to database at localhost:5432
Is PostgreSQL running? Try: pg_isready
```

### Python 구현

```python
import sys

def handle_error(error, suggestion=None):
    message = f"Error: {error}"
    if suggestion:
        message += f"\n→ {suggestion}"
    print(message, file=sys.stderr)
    sys.exit(1)

# 사용
try:
    open("config.yaml")
except FileNotFoundError:
    handle_error(
        "Can't read config.yaml: file not found",
        "Create it with: myapp init"
    )
```

## 오류 그룹화

같은 유형의 여러 오류는 그룹화합니다:

```bash
# 나쁨 (하나씩)
Error: file1.txt not found
Error: file2.txt not found
Error: file3.txt not found

# 좋음 (그룹화)
Error: 3 files not found
  - file1.txt
  - file2.txt
  - file3.txt
```

## 신호-대잡음비

중요 정보를 끝에 배치합니다:

```bash
# 나쁨 (중요 정보가 위에)
Error: Database connection failed
Connecting to database...
Loading config...
Starting application...

# 좋음 (중요 정보가 아래)
Loading config...
Starting application...
Connecting to database...
Error: Database connection failed
→ Check if PostgreSQL is running
```

## 디버그 정보

### 예기치 않은 오류

스택 트레이스와 버그 리포트 방법을 제공:

```bash
$ myapp run
Unexpected error occurred!

If this is a bug, please report it:
https://github.com/user/myapp/issues/new

Debug info:
  Version: 1.0.0
  OS: macOS 14.0
  Python: 3.12.0

Stack trace:
  File "main.py", line 42, in run
    ...
```

### Python 구현

```python
import sys
import traceback
import platform

def handle_unexpected_error(error):
    print("Unexpected error occurred!", file=sys.stderr)
    print(file=sys.stderr)
    print("If this is a bug, please report it:", file=sys.stderr)
    print("https://github.com/user/myapp/issues/new", file=sys.stderr)
    print(file=sys.stderr)
    print("Debug info:", file=sys.stderr)
    print(f"  Version: {__version__}", file=sys.stderr)
    print(f"  OS: {platform.system()} {platform.release()}", file=sys.stderr)
    print(f"  Python: {platform.python_version()}", file=sys.stderr)
    print(file=sys.stderr)

    if os.environ.get('DEBUG'):
        traceback.print_exc()

    sys.exit(1)
```

## 버그 리포트 용이화

URL에 정보를 사전 채워 제공:

```python
import urllib.parse

def bug_report_url(error_msg, version, os_info):
    base = "https://github.com/user/myapp/issues/new"
    params = {
        "title": f"Bug: {error_msg[:50]}",
        "body": f"""
## Environment
- Version: {version}
- OS: {os_info}

## Error
{error_msg}

## Steps to reproduce
1.
2.
3.
"""
    }
    return f"{base}?{urllib.parse.urlencode(params)}"
```

## 경고 vs 오류

```bash
# 경고: 계속 진행 가능
Warning: config.yaml not found, using defaults

# 오류: 진행 불가
Error: database connection failed
```

### 경고 레벨

```bash
# 기본: 오류만
$ myapp run
Error: ...

# -v: 경고 포함
$ myapp run -v
Warning: ...
Error: ...

# -vv: 정보 포함
$ myapp run -vv
Info: ...
Warning: ...
Error: ...

# -vvv: 디버그 포함
$ myapp run -vvv
Debug: ...
Info: ...
Warning: ...
Error: ...
```

## 오류 색상

```python
import sys

RED = '\033[31m'
YELLOW = '\033[33m'
RESET = '\033[0m'

def error(msg):
    print(f"{RED}Error:{RESET} {msg}", file=sys.stderr)

def warning(msg):
    print(f"{YELLOW}Warning:{RESET} {msg}", file=sys.stderr)
```

## 공통 오류 패턴

### 파일 오류

```bash
Can't read input.txt: file not found
Can't write output.txt: permission denied
Can't parse config.yaml: invalid syntax at line 5
```

### 네트워크 오류

```bash
Can't connect to api.example.com: connection refused
Request timeout after 30s. Check your network connection.
SSL certificate expired for api.example.com
```

### 인자 오류

```bash
Missing required argument: --output
Unknown flag: --quite (did you mean --quiet?)
Invalid value for --count: expected number, got 'abc'
```
