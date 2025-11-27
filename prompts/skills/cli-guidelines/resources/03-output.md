# 출력 가이드라인

## stdout vs stderr

```
stdout: 주요 출력 (프로그램의 결과물)
stderr: 메시지, 오류, 로그, 진행률
```

### 규칙

```bash
# stdout: 파이프로 전달될 데이터
myapp list > files.txt

# stderr: 사용자를 위한 메시지
Processing... (stderr)
file1.txt    (stdout)
file2.txt    (stdout)
Done!        (stderr)
```

### Python 예시

```python
import sys

# 주요 출력 → stdout
print(result)

# 메시지/로그 → stderr
print("Processing...", file=sys.stderr)
```

## TTY 감지

터미널(TTY) 여부로 출력 형식을 결정합니다:

```python
import sys

if sys.stdout.isatty():
    # 사람용: 색상, 포맷팅, 프로그레스 바
    print("\033[32mSuccess!\033[0m")
else:
    # 기계용: plain text
    print("Success!")
```

## 성공 시 출력

전통적 UNIX는 성공 시 무출력이지만, 사람이 사용할 때는 확인 메시지가 좋습니다:

```bash
# 너무 조용함
$ deploy
$

# 적절함
$ deploy
Deployed to production!
View at: https://example.com
```

`-q/--quiet` 옵션으로 억제 가능하게:

```bash
$ deploy --quiet
$
```

## JSON 출력

`--json` 플래그로 구조화된 데이터를 제공합니다:

```bash
# 사람용 (기본)
$ myapp status
Name:    myapp
Version: 1.0.0
Status:  running

# 기계용
$ myapp status --json
{"name": "myapp", "version": "1.0.0", "status": "running"}
```

### jq 친화적 출력

```bash
# 한 줄씩 JSON 객체 (NDJSON)
$ myapp list --json
{"name": "file1", "size": 1024}
{"name": "file2", "size": 2048}

# jq로 처리
$ myapp list --json | jq '.name'
```

## 테이블 출력

`--plain` 플래그로 탭 구분 출력:

```bash
# 사람용 (기본)
$ myapp list
NAME     SIZE     DATE
file1    1.0K     2024-01-01
file2    2.0K     2024-01-02

# 기계용
$ myapp list --plain
file1	1024	2024-01-01
file2	2048	2024-01-02
```

## 색상 사용

### 의미 있는 색상

```
빨강:  오류
노랑:  경고
초록:  성공
파랑:  정보
회색:  부가 정보
```

### 색상 비활성화 조건

다음 조건에서 색상을 끕니다:

```python
import os
import sys

def should_use_color():
    # 1. TTY가 아님
    if not sys.stdout.isatty():
        return False

    # 2. NO_COLOR 환경변수
    if os.environ.get('NO_COLOR'):
        return False

    # 3. TERM=dumb
    if os.environ.get('TERM') == 'dumb':
        return False

    # 4. --no-color 플래그 (인자에서 체크)

    return True
```

### 색상 라이브러리

- Python: `rich`, `colorama`, `termcolor`
- Rust: `colored`, `termcolor`
- Go: `fatih/color`
- Node.js: `chalk`

## 진행률 표시

장시간 작업에는 진행률을 표시합니다:

```bash
# 스피너
Processing... ⠋

# 프로그레스 바
Downloading [████████░░░░░░░░] 50% 1.2MB/s

# 카운터
Processing 50/100 files...
```

### TTY가 아닐 때

CI/CD 등 비대화형 환경에서는 애니메이션을 비활성화:

```python
if sys.stdout.isatty():
    # 애니메이션 프로그레스 바
    with tqdm(items) as pbar:
        for item in pbar:
            process(item)
else:
    # 단순 로그
    for i, item in enumerate(items):
        print(f"Processing {i+1}/{len(items)}", file=sys.stderr)
        process(item)
```

## 다음 명령 제안

워크플로우에서 다음 단계를 암시합니다:

```bash
$ git add file.txt
$ git status
Changes to be committed:
  new file:   file.txt

# 다음 단계 제안
(use "git commit" to commit the staged changes)
```

## 페이저 사용

긴 출력은 페이저로 표시:

```bash
# 직접 호출
myapp log | less

# 자동 페이징
$ myapp log  # 길면 자동으로 less 호출
```

### Python 예시

```python
import subprocess
import sys

def pager(content):
    if not sys.stdout.isatty():
        print(content)
        return

    pager_cmd = os.environ.get('PAGER', 'less -FIRX')
    proc = subprocess.Popen(pager_cmd.split(), stdin=subprocess.PIPE)
    proc.communicate(input=content.encode())
```

## 기호와 이모지

의미를 명확히 하는 기호를 사용합니다:

```bash
✓ Task completed
✗ Task failed
⚠ Warning: disk space low
→ Next step: run 'myapp deploy'
```

비활성화 조건 (접근성):
- `--no-unicode` 플래그
- `LANG=C` 환경
- 터미널이 유니코드 미지원

## 디버그 정보

기본값에서는 개발자용 정보를 숨깁니다:

```bash
# 기본
$ myapp run
Error: Connection failed

# 상세 모드
$ myapp run --verbose
[DEBUG] Connecting to api.example.com:443
[DEBUG] TLS handshake...
Error: Connection failed
  → SSL certificate expired
```
