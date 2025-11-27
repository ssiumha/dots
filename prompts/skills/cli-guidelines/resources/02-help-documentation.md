# 도움말과 문서화

## 기본 규칙

### 필수 플래그

```bash
-h, --help     # 도움말 표시
--version      # 버전 표시
```

모든 서브커맨드도 자체 `--help`를 지원해야 합니다.

```bash
myapp --help           # 전체 도움말
myapp subcommand --help  # 서브커맨드 도움말
```

## 도움말 구조

### 인자 없이 실행 시

간결한 초기 도움말을 표시합니다:
- 프로그램 설명 (1-2줄)
- 기본 사용법 예제 1-2개
- 자주 쓰는 플래그 몇 가지
- 전체 도움말 안내

```bash
$ myapp

File converter - Convert between various file formats

Usage:
  myapp <input> -o <output>
  myapp --help for more options

Examples:
  myapp data.csv -o data.json
  myapp image.png -o image.webp
```

### --help 출력

포괄적인 도움말을 제공합니다:

```bash
$ myapp --help

myapp - File converter

USAGE
  myapp <input> [options]
  myapp <command> [args]

COMMANDS
  convert    Convert file format
  validate   Validate file structure
  info       Show file information

OPTIONS
  -o, --output <file>    Output file path
  -f, --format <fmt>     Output format (json, csv, xml)
  -v, --verbose          Show detailed output
  -q, --quiet            Suppress non-error output
  -h, --help             Show this help
  --version              Show version

EXAMPLES
  # Convert CSV to JSON
  myapp data.csv -o data.json

  # Convert with specific format
  myapp data.csv -f xml -o data.xml

  # Validate file
  myapp validate data.json

DOCS
  https://myapp.dev/docs

FEEDBACK
  https://github.com/user/myapp/issues
```

## 도움말 작성 원칙

### 1. 예제 우선

예제를 먼저 보여주고, 옵션 설명은 나중에:

```bash
# 좋음
EXAMPLES
  myapp deploy --env production
  myapp deploy --env staging --dry-run

# 나쁨 (옵션만 나열)
OPTIONS
  --env     Environment
  --dry-run Dry run mode
```

### 2. 모든 플래그에 롱 버전 제공

```bash
# 좋음
-v, --verbose
-q, --quiet
-f, --force

# 나쁨 (숏 버전만)
-v
-q
-f
```

롱 버전은 스크립트에서 명확성을 높입니다:
```bash
# 명확함
myapp --force --verbose

# 불명확함
myapp -fv
```

### 3. 포맷팅 활용

- 볼드 제목과 섹션 구분
- 적절한 들여쓰기
- 터미널 폭 고려 (80자 권장)

## 오류 제안

사용자 입력 오류 시 수정안을 제시합니다:

```bash
$ myapp comit
Unknown command: comit
Did you mean: commit?

$ myapp --quite
Unknown flag: --quite
Did you mean: --quiet?
```

### 구현 예시 (Python)

```python
import difflib

def suggest_command(user_input, valid_commands):
    matches = difflib.get_close_matches(user_input, valid_commands, n=1)
    if matches:
        return f"Did you mean: {matches[0]}?"
    return None
```

## stdin 대기 방지

파이프를 기대하는 명령이 터미널에서 실행되면 즉시 도움말을 표시합니다:

```python
import sys

def main():
    if sys.stdin.isatty() and len(sys.argv) == 1:
        # stdin이 없고 인자도 없음 → 도움말 표시
        print("Usage: myapp <input>")
        print("       cat data.txt | myapp")
        sys.exit(0)

    # stdin이나 파일 처리
    process_input()
```

## 문서 링크

도움말에서 웹 문서로의 직접 링크를 제공합니다:

```bash
$ myapp --help
...
DOCUMENTATION
  Full docs: https://myapp.dev/docs
  Tutorial:  https://myapp.dev/tutorial

FEEDBACK
  Issues:    https://github.com/user/myapp/issues
  Discord:   https://discord.gg/myapp
```

## man 페이지

복잡한 CLI는 man 페이지도 제공합니다:

```bash
man myapp
myapp --help  # 간단 버전
```

도구로 자동 생성 가능:
- Python: `click-man`, `argparse-manpage`
- Rust: `clap_mangen`
- Go: `cobra/doc`
