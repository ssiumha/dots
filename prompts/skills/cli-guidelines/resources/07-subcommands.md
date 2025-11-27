# 서브커맨드

## 기본 구조

```bash
myapp <command> [subcommand] [options] [args]
```

예시:
```bash
git commit -m "message"
docker container ls
kubectl get pods
```

## 일관성

### 플래그 일관성

서브커맨드 간 동일한 플래그는 동일하게 동작:

```bash
# 모든 서브커맨드에서 같은 의미
myapp build --verbose
myapp deploy --verbose
myapp test --verbose
```

### 출력 형식 일관성

```bash
# 모든 list 명령이 같은 형식
myapp users list
myapp files list
myapp projects list
```

## 명확한 이름

### 혼동 가능한 이름 피하기

```bash
# 나쁨: update와 upgrade의 차이가 불명확
myapp update
myapp upgrade

# 좋음: 명확한 구분
myapp sync          # 상태 동기화
myapp upgrade       # 버전 업그레이드
```

### 동사 vs 명사

일관된 패턴 선택:

```bash
# verb-noun (동사-명사)
git add file
git commit changes
git push origin

# noun-verb (명사-동사)
docker container create
docker container start
docker container stop
```

**한 가지 패턴을 선택하고 일관성 유지**

## 축약 금지

서브커맨드 축약을 허용하지 마세요:

```bash
# 나쁨: 축약 허용
myapp i        # install의 축약

# 문제: 나중에 'import'를 추가할 수 없음
myapp import   # 'i'가 이미 사용됨

# 좋음: 명시적 이름만 허용
myapp install
```

## 기본 서브커맨드 금지

서브커맨드 없이 실행할 때 기본 동작을 하지 마세요:

```bash
# 나쁨: 기본 서브커맨드
myapp          # 암묵적으로 'myapp run' 실행

# 문제: 나중에 호환성 문제
myapp          # 이게 run? status? help?

# 좋음: 명시적 요구
myapp run
myapp status
myapp --help   # 인자 없이는 도움말
```

## 구조 예시

### 단순 구조

```bash
myapp init
myapp build
myapp deploy
myapp --help
myapp --version
```

### 리소스 기반

```bash
myapp users list
myapp users create
myapp users delete <id>

myapp projects list
myapp projects create
myapp projects delete <id>
```

### 다단계

```bash
myapp config get <key>
myapp config set <key> <value>
myapp config list

myapp cluster node add
myapp cluster node remove
myapp cluster node list
```

## 공통 플래그 위치

전역 플래그는 서브커맨드 전/후 모두 허용:

```bash
# 둘 다 동작
myapp --verbose deploy
myapp deploy --verbose
```

### Python (Click) 구현

```python
import click

@click.group()
@click.option('--verbose', '-v', is_flag=True)
@click.pass_context
def cli(ctx, verbose):
    ctx.ensure_object(dict)
    ctx.obj['verbose'] = verbose

@cli.command()
@click.pass_context
def deploy(ctx):
    if ctx.obj['verbose']:
        click.echo("Verbose mode enabled")
    click.echo("Deploying...")
```

## 도움말 구조

```bash
$ myapp --help
Usage: myapp <command> [options]

Commands:
  init      Initialize a new project
  build     Build the project
  deploy    Deploy to production
  config    Manage configuration

Run 'myapp <command> --help' for more information.

$ myapp config --help
Usage: myapp config <subcommand> [options]

Subcommands:
  get <key>           Get config value
  set <key> <value>   Set config value
  list                List all config

Examples:
  myapp config get api.url
  myapp config set api.url https://api.example.com
```

## 별칭 (Alias)

자주 쓰는 명령에 별칭 제공:

```bash
# 공식 이름
myapp container list

# 별칭
myapp containers     # container list의 별칭
myapp ps             # container list의 별칭
```

### 별칭 표시

도움말에 별칭 명시:

```bash
Commands:
  container list   List containers (aliases: containers, ps)
```

## CRUD 패턴

리소스 관리 명령은 일관된 CRUD 패턴:

```bash
myapp <resource> list              # Read (목록)
myapp <resource> get <id>          # Read (단일)
myapp <resource> create [options]  # Create
myapp <resource> update <id>       # Update
myapp <resource> delete <id>       # Delete
```

또는 약식:

```bash
myapp <resource>s                  # list 별칭
myapp <resource> <id>              # get 별칭
```

## 플래그와 인자 구분

```bash
# 서브커맨드 고유 플래그
myapp deploy --env production

# 전역 플래그
myapp --verbose deploy

# 위치 인자
myapp deploy production            # 가능하지만 플래그 권장
```

## 오류 처리

알 수 없는 서브커맨드에 제안:

```bash
$ myapp delpoy
Unknown command: delpoy
Did you mean: deploy?

Run 'myapp --help' for available commands.
```
