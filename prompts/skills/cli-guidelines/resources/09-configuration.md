# 설정 관리

## 설정 위치 선택

### 기준: 특이성, 안정성, 복잡성

| 유형 | 위치 | 예시 |
|------|------|------|
| 매번 변할 수 있음 | 플래그 | `--verbose`, `--dry-run` |
| 일반적 안정, 프로젝트별 다름 | 환경변수 | `NO_COLOR`, `HTTP_PROXY` |
| 프로젝트 내 안정, 버전 관리 | 프로젝트 파일 | `.myapprc`, `myapp.config.js` |

## 설정 우선순위

1. **플래그** (최우선)
2. **환경변수**
3. **프로젝트 설정** (`.env`, 현재 디렉토리)
4. **사용자 설정** (`~/.config/myapp/`)
5. **시스템 설정** (`/etc/myapp/`) (최하위)

### Python 구현

```python
import os

def get_config(key, default=None):
    # 1. 환경변수 (플래그는 argparse에서 처리)
    env_key = f"MYAPP_{key.upper()}"
    if env_key in os.environ:
        return os.environ[env_key]

    # 2. 프로젝트 설정
    project_config = load_project_config()
    if key in project_config:
        return project_config[key]

    # 3. 사용자 설정
    user_config = load_user_config()
    if key in user_config:
        return user_config[key]

    return default
```

## XDG 기반 구조

홈 디렉토리를 깔끔하게 유지:

```
~/.config/myapp/          # 설정 파일
~/.local/share/myapp/     # 데이터 파일
~/.cache/myapp/           # 캐시 파일
```

### Python 구현

```python
import os
from pathlib import Path

def get_config_dir():
    xdg = os.environ.get('XDG_CONFIG_HOME')
    if xdg:
        return Path(xdg) / 'myapp'
    return Path.home() / '.config' / 'myapp'

def get_data_dir():
    xdg = os.environ.get('XDG_DATA_HOME')
    if xdg:
        return Path(xdg) / 'myapp'
    return Path.home() / '.local' / 'share' / 'myapp'

def get_cache_dir():
    xdg = os.environ.get('XDG_CACHE_HOME')
    if xdg:
        return Path(xdg) / 'myapp'
    return Path.home() / '.cache' / 'myapp'
```

## 환경변수

### 이름 규칙

- 대문자만 사용
- 언더스코어로 구분
- 프로그램 접두사

```bash
MYAPP_DEBUG=1
MYAPP_CONFIG_PATH=/path/to/config
MYAPP_API_URL=https://api.example.com
```

### 표준 환경변수 확인

| 변수 | 용도 |
|------|------|
| `NO_COLOR` | 색상 비활성화 |
| `FORCE_COLOR` | 색상 강제 활성화 |
| `DEBUG` | 디버그 모드 |
| `EDITOR` | 기본 편집기 |
| `PAGER` | 페이저 (less, more) |
| `HOME` | 홈 디렉토리 |
| `TMPDIR` | 임시 파일 디렉토리 |
| `HTTP_PROXY` | HTTP 프록시 |
| `HTTPS_PROXY` | HTTPS 프록시 |
| `TERM` | 터미널 유형 |
| `SHELL` | 기본 셸 |
| `LANG` | 로케일 |
| `LINES`, `COLUMNS` | 터미널 크기 |

### 사용 예시

```python
import os

# 색상 제어
use_color = (
    os.environ.get('FORCE_COLOR') or
    (not os.environ.get('NO_COLOR') and sys.stdout.isatty())
)

# 편집기
editor = os.environ.get('EDITOR', 'vim')

# 프록시
proxies = {}
if os.environ.get('HTTP_PROXY'):
    proxies['http'] = os.environ['HTTP_PROXY']
if os.environ.get('HTTPS_PROXY'):
    proxies['https'] = os.environ['HTTPS_PROXY']
```

## .env 파일

프로젝트별 환경변수:

```bash
# .env
MYAPP_DEBUG=1
MYAPP_API_URL=https://api.example.com
```

### Python 구현

```python
from pathlib import Path

def load_dotenv():
    env_file = Path('.env')
    if not env_file.exists():
        return

    with open(env_file) as f:
        for line in f:
            line = line.strip()
            if not line or line.startswith('#'):
                continue
            key, _, value = line.partition('=')
            if key and key not in os.environ:  # 기존 환경변수 우선
                os.environ[key] = value
```

또는 `python-dotenv` 라이브러리 사용:

```python
from dotenv import load_dotenv
load_dotenv()
```

### .env 한계

- 단순 문자열만 지원
- 버전 관리 어려움 (비밀 포함 시)
- 복잡한 설정은 별도 파일 권장

## 설정 파일 형식

### TOML (권장)

```toml
# myapp.toml
[server]
host = "localhost"
port = 8080

[logging]
level = "info"
format = "json"
```

### YAML

```yaml
# myapp.yaml
server:
  host: localhost
  port: 8080

logging:
  level: info
  format: json
```

### JSON

```json
{
  "server": {
    "host": "localhost",
    "port": 8080
  }
}
```

## 설정 자동 수정

기존 설정을 수정할 때 사용자 동의:

```bash
$ myapp config set api.url https://new-api.example.com

This will modify ~/.config/myapp/config.toml:
  api.url: https://old-api.example.com → https://new-api.example.com

Proceed? [Y/n] y
Configuration updated.
```

## 비밀 처리

**환경변수에서 비밀을 읽지 마세요**:

```bash
# 나쁨: 환경변수
export MYAPP_SECRET=supersecret
myapp run  # 자식 프로세스에 노출됨

# 좋음: 파일에서 읽기
myapp run --secret-file ~/.myapp/secret

# 좋음: stdin에서 읽기
cat ~/.myapp/secret | myapp run --secret-stdin
```

### 안전한 비밀 저장

```python
import os
from pathlib import Path

def read_secret(path):
    secret_path = Path(path).expanduser()

    # 파일 권한 확인
    mode = secret_path.stat().st_mode
    if mode & 0o077:  # 그룹/기타 권한 있음
        print(f"Warning: {path} has insecure permissions", file=sys.stderr)

    return secret_path.read_text().strip()
```

## 설정 검증

```python
def validate_config(config):
    errors = []

    if 'port' in config:
        if not isinstance(config['port'], int):
            errors.append("port must be an integer")
        elif not (1 <= config['port'] <= 65535):
            errors.append("port must be between 1 and 65535")

    if 'log_level' in config:
        valid_levels = ['debug', 'info', 'warning', 'error']
        if config['log_level'] not in valid_levels:
            errors.append(f"log_level must be one of: {valid_levels}")

    return errors
```

## 설정 명령

```bash
# 값 조회
myapp config get api.url

# 값 설정
myapp config set api.url https://api.example.com

# 전체 목록
myapp config list

# 파일 위치
myapp config path

# 초기화
myapp config init
```
