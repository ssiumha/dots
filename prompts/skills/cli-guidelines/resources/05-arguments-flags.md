# 인자와 플래그

## 기본 규칙

### 플래그 선호

위치 인자보다 이름 있는 플래그가 명확합니다:

```bash
# 모호함
myapp input.txt output.txt json

# 명확함
myapp input.txt --output output.txt --format json
```

### 숏/롱 버전 모두 제공

```bash
-v, --verbose     # 상세 출력
-q, --quiet       # 조용한 모드
-f, --force       # 강제 실행
-o, --output      # 출력 파일
-h, --help        # 도움말
```

롱 버전은 스크립트에서 명확성을 높입니다:
```bash
# 스크립트에서 권장
myapp --force --verbose --output result.txt

# 대화형에서 편리
myapp -fv -o result.txt
```

## 표준 플래그

| 플래그 | 용도 | 설명 |
|--------|------|------|
| `-h`, `--help` | 도움말 | 사용법 표시 |
| `--version` | 버전 | 버전 정보 표시 |
| `-v`, `--verbose` | 상세 출력 | 더 많은 정보 표시 |
| `-q`, `--quiet` | 조용한 모드 | 최소 출력 |
| `-f`, `--force` | 강제 실행 | 확인 없이 실행 |
| `-n`, `--dry-run` | 시뮬레이션 | 실제 실행 없이 표시 |
| `-o`, `--output` | 출력 파일 | 결과 저장 위치 |
| `--json` | JSON 출력 | 구조화된 출력 |
| `--no-color` | 색상 비활성화 | 색상 없는 출력 |
| `--no-input` | 비대화형 | 프롬프트 없이 실행 |
| `-a`, `--all` | 모든 항목 | 전체 대상 |
| `-d`, `--debug` | 디버그 | 디버그 정보 표시 |

## 플래그 값 형식

```bash
# 공백 구분 (권장)
--output file.txt
-o file.txt

# 등호 구분 (허용)
--output=file.txt

# 숏 플래그 붙여쓰기 (허용)
-ofile.txt
```

## 다중 값

```bash
# 다중 인자
rm file1.txt file2.txt file3.txt

# 다중 플래그
--include "*.js" --include "*.ts"

# 콤마 구분
--types js,ts,json
```

## 불리언 플래그

```bash
# 활성화 (값 없음)
--verbose
--force

# 비활성화 (--no- 접두사)
--no-color
--no-cache
--no-verify
```

### Python 구현

```python
import argparse

parser = argparse.ArgumentParser()
parser.add_argument('--color', action='store_true', default=True)
parser.add_argument('--no-color', dest='color', action='store_false')
```

## 합리적 기본값

대다수 사용자에게 최적의 기본 설정을 제공합니다:

```bash
# 색상: 기본 활성화 (TTY일 때)
# 출력 형식: 기본 사람용
# 상세도: 기본 보통

myapp run  # 대부분의 경우 추가 플래그 불필요
```

## 입력 프롬프트

플래그 미제공 시 대화형으로 입력을 요청합니다:

```bash
$ myapp deploy
Environment (production/staging): production
Branch [main]:
Deploying main to production...
```

### 규칙

1. **TTY 확인 필수**: 파이프에서는 프롬프트 불가
2. **항상 플래그로 대체 가능**: 비대화형 지원
3. **기본값 표시**: `[main]`처럼 표시

```python
import sys

def prompt_if_tty(message, default=None):
    if not sys.stdin.isatty():
        if default is not None:
            return default
        raise ValueError(f"Missing required input: {message}")

    if default:
        result = input(f"{message} [{default}]: ")
        return result or default
    return input(f"{message}: ")
```

## 위험 작업 확인

파일 삭제 등 위험한 작업은 확인 단계를 추가합니다:

### 심각도별 처리

```bash
# 경미: --force로 스킵 가능
$ myapp clean
Remove 5 files? [y/N] y

$ myapp clean --force
Removed 5 files.

# 중간: 확인 프롬프트 필수
$ myapp delete-database
WARNING: This will delete ALL data!
Type 'delete' to confirm: delete
Database deleted.

# 심각: 이름 입력 강제
$ myapp delete-account
Enter account name to confirm: my-account
Account 'my-account' deleted.
```

## stdin/stdout에 `-` 지원

```bash
# stdin에서 읽기
cat data.txt | myapp -
myapp - < data.txt

# stdout으로 쓰기
myapp input.txt -o -
myapp input.txt -o - | other-tool
```

### Python 구현

```python
import sys

def get_input(path):
    if path == '-':
        return sys.stdin.read()
    with open(path) as f:
        return f.read()

def write_output(path, content):
    if path == '-':
        sys.stdout.write(content)
    else:
        with open(path, 'w') as f:
            f.write(content)
```

## 순서 독립성

가능한 한 플래그 순서와 상관없이 작동:

```bash
# 모두 동일하게 동작
myapp --verbose --output file.txt input.txt
myapp input.txt --output file.txt --verbose
myapp -o file.txt -v input.txt
```

## 비밀 처리

**절대 비밀을 플래그로 전달하지 마세요**:

```bash
# 나쁨: ps, history에 노출
myapp --password secret123

# 좋음: 파일에서 읽기
myapp --password-file ~/.myapp/credentials

# 좋음: stdin에서 읽기
echo "secret" | myapp --password-stdin

# 좋음: 환경변수 (주의: 자식 프로세스에 노출)
MYAPP_PASSWORD=secret myapp run

# 좋음: 대화형 프롬프트 (에코 비활성화)
$ myapp login
Password: ********
```

### Python 구현 (getpass)

```python
import getpass

password = getpass.getpass("Password: ")
```

## 특수 값 처리

```bash
# "none"으로 비활성화
--color none
--proxy none

# 빈 값 명시
--prefix ""
```

## 단일 문자 플래그 제한

자주 사용되는 것만 단일 문자로:
- `-h`, `-v`, `-q`, `-f`, `-o`, `-n`

네임스페이스 오염 방지:
```bash
# 나쁨: 모든 플래그에 숏 버전
-a, -b, -c, -d, -e, -f, ...

# 좋음: 자주 쓰는 것만
-v, --verbose
--some-rare-option  # 숏 버전 없음
```
