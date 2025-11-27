# 견고성

## 반응성

### 100ms 규칙

**100ms 이내에 무언가를 표시**해야 사용자가 정지 상태가 아님을 인지합니다:

```python
import sys

# 네트워크 요청 전 메시지 출력
print("Connecting to server...", file=sys.stderr)
response = requests.get(url)  # 시간이 걸릴 수 있음
```

### 즉각적 피드백

```bash
$ myapp deploy
Connecting to server...      # 즉시 표시
Uploading files...           # 진행 상황
Deploying...
Done!
```

## 진행률 표시

장시간 작업에는 "살아있음" 신호를 보냅니다:

### 스피너

```python
import itertools
import sys
import time
import threading

def spinner(message):
    chars = itertools.cycle(['⠋', '⠙', '⠹', '⠸', '⠼', '⠴', '⠦', '⠧', '⠇', '⠏'])
    while not spinner.done:
        sys.stderr.write(f'\r{message} {next(chars)}')
        sys.stderr.flush()
        time.sleep(0.1)
    sys.stderr.write('\r' + ' ' * 50 + '\r')

spinner.done = False

# 사용
t = threading.Thread(target=spinner, args=("Processing...",))
t.start()
# ... 작업 ...
spinner.done = True
t.join()
```

### 프로그레스 바

```bash
Downloading [████████████░░░░░░░░] 60% 1.2MB/s ETA 5s
```

```python
from tqdm import tqdm

for item in tqdm(items, desc="Processing"):
    process(item)
```

### 카운터

```bash
Processing 50/100 files...
```

## TTY가 아닐 때

CI/CD 등 비대화형 환경에서는 애니메이션을 비활성화:

```python
import sys

if sys.stdout.isatty():
    # 애니메이션 프로그레스 바
    from tqdm import tqdm
    for item in tqdm(items):
        process(item)
else:
    # 단순 로그
    for i, item in enumerate(items):
        if i % 10 == 0:  # 10개마다 출력
            print(f"Processed {i}/{len(items)}", file=sys.stderr)
        process(item)
    print(f"Processed {len(items)}/{len(items)}", file=sys.stderr)
```

## 시그널 처리

### Ctrl-C (SIGINT)

빠르게 반응하여 종료:

```python
import signal
import sys

def handle_sigint(signum, frame):
    print("\nInterrupted.", file=sys.stderr)
    sys.exit(130)  # 128 + 2 (SIGINT)

signal.signal(signal.SIGINT, handle_sigint)
```

### 정리 작업

정리 작업에 타임아웃 설정:

```python
import signal
import sys

cleanup_in_progress = False

def handle_sigint(signum, frame):
    global cleanup_in_progress

    if cleanup_in_progress:
        # 두 번째 Ctrl-C: 즉시 종료
        print("\nForce quit.", file=sys.stderr)
        sys.exit(1)

    cleanup_in_progress = True
    print("\nCleaning up... (Ctrl-C again to force quit)", file=sys.stderr)

    try:
        cleanup()
    except:
        pass

    sys.exit(130)

signal.signal(signal.SIGINT, handle_sigint)
```

### SIGTERM

graceful shutdown:

```python
def handle_sigterm(signum, frame):
    print("Received SIGTERM, shutting down...", file=sys.stderr)
    cleanup()
    sys.exit(0)

signal.signal(signal.SIGTERM, handle_sigterm)
```

## 타임아웃

네트워크 요청에 합리적 기본값:

```python
import requests

# 연결 타임아웃: 5초, 읽기 타임아웃: 30초
response = requests.get(url, timeout=(5, 30))
```

### 사용자 설정 가능

```bash
myapp fetch --timeout 60
```

## 복구 가능성

임시 오류 시 `up arrow` + `enter`로 재개 가능하게:

```bash
$ myapp upload file.txt
Uploading... 50%
Error: Connection lost

# 재실행으로 이어서 가능
$ myapp upload file.txt
Resuming from 50%...
Uploading... 100%
Done!
```

### 체크포인트 저장

```python
import json
import os

CHECKPOINT_FILE = ".myapp_checkpoint"

def save_checkpoint(state):
    with open(CHECKPOINT_FILE, 'w') as f:
        json.dump(state, f)

def load_checkpoint():
    if os.path.exists(CHECKPOINT_FILE):
        with open(CHECKPOINT_FILE) as f:
            return json.load(f)
    return None

def clear_checkpoint():
    if os.path.exists(CHECKPOINT_FILE):
        os.remove(CHECKPOINT_FILE)
```

## 크래시 전용 설계

정리 작업 최소화로 즉시 종료:

```bash
# 좋음: 즉시 종료
$ myapp run
^C
Interrupted.
$

# 나쁨: 정리에 시간 소요
$ myapp run
^C
Cleaning up...
Saving state...
Closing connections...
$
```

### 원칙

1. 언제든 종료 가능하게 설계
2. 시작 시 불완전한 상태 처리
3. 임시 파일은 시스템 temp 사용

```python
import tempfile
import atexit
import os

# 시스템 temp 사용 - OS가 정리
temp_dir = tempfile.mkdtemp()

def cleanup():
    import shutil
    if os.path.exists(temp_dir):
        shutil.rmtree(temp_dir)

atexit.register(cleanup)
```

## 병렬 처리

출력 얽힘 주의:

```python
import concurrent.futures
import threading

print_lock = threading.Lock()

def safe_print(msg):
    with print_lock:
        print(msg)

def process_item(item):
    # 작업 수행
    result = do_work(item)
    safe_print(f"Processed: {item}")
    return result

with concurrent.futures.ThreadPoolExecutor() as executor:
    results = executor.map(process_item, items)
```

## 입력 검증

예상 오류는 미리 포착:

```python
def validate_inputs(args):
    errors = []

    if not os.path.exists(args.input):
        errors.append(f"Input file not found: {args.input}")

    if args.output and os.path.exists(args.output) and not args.force:
        errors.append(f"Output file exists: {args.output} (use --force to overwrite)")

    if errors:
        for error in errors:
            print(f"Error: {error}", file=sys.stderr)
        sys.exit(1)

# 작업 시작 전에 모든 검증
validate_inputs(args)
process(args)
```

## 예상 외 사용 대응

### 스크립트에서 호출

```bash
#!/bin/bash
# 스크립트에서 myapp 호출
result=$(myapp process file.txt)
if [ $? -ne 0 ]; then
    echo "Failed"
    exit 1
fi
```

### 여러 인스턴스

```python
import fcntl
import sys

def acquire_lock():
    lock_file = open('/tmp/myapp.lock', 'w')
    try:
        fcntl.flock(lock_file, fcntl.LOCK_EX | fcntl.LOCK_NB)
        return lock_file
    except IOError:
        print("Another instance is running", file=sys.stderr)
        sys.exit(1)
```

### 불안정한 네트워크

```python
import time
from functools import wraps

def retry(max_attempts=3, delay=1):
    def decorator(func):
        @wraps(func)
        def wrapper(*args, **kwargs):
            for attempt in range(max_attempts):
                try:
                    return func(*args, **kwargs)
                except Exception as e:
                    if attempt == max_attempts - 1:
                        raise
                    print(f"Retrying in {delay}s...", file=sys.stderr)
                    time.sleep(delay)
        return wrapper
    return decorator

@retry(max_attempts=3)
def fetch_data(url):
    return requests.get(url)
```
