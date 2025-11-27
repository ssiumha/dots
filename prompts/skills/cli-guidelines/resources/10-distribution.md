# 배포와 명명

## 명명 규칙

### 좋은 이름의 특성

1. **간단하고 기억하기 쉬움**
2. **소문자 사용**
3. **대시 최소화**
4. **짧되 의미 있게**
5. **타이핑 용이성**

### 예시

```bash
# 좋음
curl
git
npm
jq
rg (ripgrep)

# 나쁨
DownloadURL     # 대소문자 혼합
my-super-tool   # 너무 긴 대시
doeverything    # 불명확
```

### 충돌 피하기

기존 명령과 충돌하지 않도록:

```bash
# 충돌 가능
convert          # ImageMagick과 충돌
test             # 셸 내장 명령과 충돌
install          # 시스템 명령과 충돌

# 안전
myapp-convert
imgconvert
```

### 타이핑 용이성

```bash
# 한 손으로 쉽게
fig, hub, rg

# 어려움 (손 호핑)
plum, qwop
```

## 단일 바이너리 배포

의존성 없이 단일 실행 파일 배포:

### 언어별 도구

| 언어 | 도구 |
|------|------|
| Python | PyInstaller, Nuitka, PyOxidizer |
| Go | 기본 컴파일러 (단일 바이너리 생성) |
| Rust | 기본 컴파일러 (단일 바이너리 생성) |
| Node.js | pkg, nexe |

### Python 예시 (PyInstaller)

```bash
pip install pyinstaller
pyinstaller --onefile myapp.py
```

## 설치 방법 제공

### 패키지 매니저

```bash
# macOS
brew install myapp

# Ubuntu/Debian
apt install myapp

# Fedora
dnf install myapp

# npm (Node.js 도구)
npm install -g myapp

# pip (Python 도구)
pip install myapp
```

### curl 설치 스크립트

```bash
curl -sSL https://myapp.dev/install.sh | bash
```

### 설치 스크립트 예시

```bash
#!/bin/bash
set -e

VERSION="1.0.0"
OS=$(uname -s | tr '[:upper:]' '[:lower:]')
ARCH=$(uname -m)

case "$ARCH" in
    x86_64) ARCH="amd64" ;;
    aarch64|arm64) ARCH="arm64" ;;
esac

URL="https://github.com/user/myapp/releases/download/v${VERSION}/myapp-${OS}-${ARCH}"

echo "Downloading myapp v${VERSION}..."
curl -sSL "$URL" -o /usr/local/bin/myapp
chmod +x /usr/local/bin/myapp

echo "myapp installed successfully!"
echo "Run 'myapp --help' to get started."
```

## 제거 용이성

설치 명령 하단에 제거 방법 명시:

```bash
$ myapp --version
myapp 1.0.0

To uninstall:
  brew uninstall myapp
  # or
  rm /usr/local/bin/myapp
```

### 제거 스크립트

```bash
#!/bin/bash
rm /usr/local/bin/myapp
rm -rf ~/.config/myapp
rm -rf ~/.cache/myapp
echo "myapp uninstalled."
```

## 버전 관리

### 시맨틱 버저닝

```
MAJOR.MINOR.PATCH
```

- **MAJOR**: 호환되지 않는 변경
- **MINOR**: 새 기능 (호환 유지)
- **PATCH**: 버그 수정

### 버전 표시

```bash
$ myapp --version
myapp 1.2.3
```

### 상세 버전

```bash
$ myapp version
myapp 1.2.3
Commit: abc1234
Built: 2024-01-01
Go: 1.21.0
```

## 자동 업데이트

### 업데이트 확인

```bash
$ myapp
A new version is available: 1.3.0 (current: 1.2.3)
Run 'myapp update' to upgrade.
```

### 업데이트 명령

```bash
$ myapp update
Downloading myapp 1.3.0...
Updated successfully!
```

### 비활성화 옵션

```bash
# 환경변수로 비활성화
MYAPP_NO_UPDATE_CHECK=1 myapp run

# 설정으로 비활성화
myapp config set update_check false
```

## 크로스 플랫폼

### 플랫폼별 빌드

```
myapp-linux-amd64
myapp-linux-arm64
myapp-darwin-amd64
myapp-darwin-arm64
myapp-windows-amd64.exe
```

### GitHub Actions 예시

```yaml
name: Release

on:
  push:
    tags: ['v*']

jobs:
  build:
    strategy:
      matrix:
        os: [ubuntu-latest, macos-latest, windows-latest]

    runs-on: ${{ matrix.os }}

    steps:
    - uses: actions/checkout@v4

    - name: Build
      run: |
        go build -o myapp-${{ matrix.os }}

    - name: Upload
      uses: actions/upload-artifact@v4
      with:
        name: myapp-${{ matrix.os }}
        path: myapp-*
```

## 셸 완성

### Bash

```bash
# /etc/bash_completion.d/myapp
_myapp_completions() {
    local cur="${COMP_WORDS[COMP_CWORD]}"
    COMPREPLY=($(compgen -W "init build deploy help" -- "$cur"))
}
complete -F _myapp_completions myapp
```

### Zsh

```zsh
# ~/.zsh/completions/_myapp
#compdef myapp

_myapp() {
    local -a commands
    commands=(
        'init:Initialize project'
        'build:Build project'
        'deploy:Deploy to production'
    )
    _describe 'command' commands
}
```

### Fish

```fish
# ~/.config/fish/completions/myapp.fish
complete -c myapp -n __fish_use_subcommand -a init -d 'Initialize project'
complete -c myapp -n __fish_use_subcommand -a build -d 'Build project'
complete -c myapp -n __fish_use_subcommand -a deploy -d 'Deploy to production'
```

## 분석 (선택적)

사용자 동의 하에만 수집:

```bash
$ myapp init
Would you like to help improve myapp by sending anonymous usage data?
This includes: commands used, OS type, errors encountered
No personal data is collected. [y/N]
```

### 옵트아웃 제공

```bash
# 비활성화
myapp config set telemetry false
MYAPP_NO_TELEMETRY=1 myapp run
```

### 대안

- 웹 문서 방문 통계
- 다운로드 카운터
- GitHub 이슈/스타
- 직접 피드백 요청
