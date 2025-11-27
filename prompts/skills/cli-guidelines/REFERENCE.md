# CLI Guidelines Reference

clig.dev 기반의 CLI 설계 가이드라인 전체 개요입니다.

## 리소스 구조

| 리소스 | 주제 | 핵심 내용 |
|--------|------|----------|
| `01-philosophy.md` | 설계 철학 | 인간 중심, 상호 연동성, 일관성, 공감 |
| `02-help-documentation.md` | 도움말 | -h/--help, 예제 우선, 오류 제안 |
| `03-output.md` | 출력 | stdout/stderr, 색상, JSON, 페이저 |
| `04-errors.md` | 오류 처리 | 친화적 메시지, 종료 코드, 디버그 정보 |
| `05-arguments-flags.md` | 인자/플래그 | 표준 플래그, 기본값, 순서 독립성 |
| `06-interactivity.md` | 상호작용 | TTY 감지, 프롬프트, 확인 단계 |
| `07-subcommands.md` | 서브커맨드 | 일관된 구조, 명확한 이름 |
| `08-robustness.md` | 견고성 | 시그널, 타임아웃, 진행률 표시 |
| `09-configuration.md` | 설정 | 환경변수, XDG, 우선순위 |
| `10-distribution.md` | 배포/명명 | 바이너리 배포, 이름 규칙 |

## 핵심 원칙 요약

### 철학

> "모든 프로그램의 결과물은 다른 미지의 프로그램의 입력이 될 것으로 예상하라." — Doug McIlroy

- **인간 중심**: CLI의 주 사용자는 사람
- **상호 연동성**: 작은 도구들의 조합
- **일관성**: UNIX 관례는 사용자의 습관
- **발견 가능성**: GUI의 이점을 CLI에 적용
- **공감**: 사용자 성공을 돕는 의도 표현

### 기본 규칙

```
성공 → exit 0
실패 → exit 1+ (0이 아닌 값)
주요 출력 → stdout
메시지/오류 → stderr
```

### 표준 플래그

| 플래그 | 용도 |
|--------|------|
| `-h`, `--help` | 도움말 |
| `-v`, `--version` | 버전 |
| `-V`, `--verbose` | 상세 출력 |
| `-q`, `--quiet` | 조용한 모드 |
| `-f`, `--force` | 강제 실행 |
| `-n`, `--dry-run` | 시뮬레이션 |
| `-o`, `--output` | 출력 파일 |
| `--json` | JSON 출력 |
| `--no-color` | 색상 비활성화 |
| `--no-input` | 상호작용 비활성화 |

### TTY 감지

```python
# Python 예시
import sys
if sys.stdout.isatty():
    # 사람용 출력 (색상, 포맷팅)
else:
    # 기계용 출력 (plain text)
```

### 색상 비활성화 조건

1. stdout/stderr가 TTY가 아님
2. `NO_COLOR` 환경변수 설정
3. `TERM=dumb`
4. `--no-color` 플래그

### 오류 메시지 패턴

```
# 나쁜 예
Error: ENOENT

# 좋은 예
Can't write to file.txt. You might need to run 'chmod +w file.txt'.
```

### 설정 우선순위

1. 플래그 (최우선)
2. 환경변수
3. 프로젝트 설정 (`.env`)
4. 사용자 설정 (`~/.config/`)
5. 시스템 설정 (최하위)

## 체크리스트

### 필수

- [ ] `-h`/`--help` 지원
- [ ] `--version` 지원
- [ ] 적절한 종료 코드 반환
- [ ] stdout/stderr 분리
- [ ] 인간 친화적 오류 메시지

### 권장

- [ ] 롱/숏 플래그 모두 제공
- [ ] 예제 포함 도움말
- [ ] TTY 감지하여 출력 조정
- [ ] `--json` 옵션 (구조화 데이터 시)
- [ ] `--quiet` 옵션
- [ ] `--no-color` 지원
- [ ] 진행률 표시 (장시간 작업)
- [ ] Ctrl-C 정상 처리

### 피해야 할 것

- [ ] 비밀을 플래그로 전달
- [ ] 플래그 없이 stdin 무한 대기
- [ ] 과도한 출력 (토큰 폭발)
- [ ] 모호한 기본 서브커맨드
- [ ] 서브커맨드 축약 (`i` → `install`)

## 언어별 CLI 라이브러리

| 언어 | 권장 라이브러리 |
|------|----------------|
| Python | Click, Typer, argparse |
| Rust | clap, structopt |
| Go | cobra, urfave/cli |
| Node.js | commander, yargs |
| Ruby | thor, optparse |

## 참고 자료

- [clig.dev](https://clig.dev) - 원본 가이드라인
- [12 Factor CLI Apps](https://medium.com/@jdxcode/12-factor-cli-apps-dd3c227a0e46)
- [Heroku CLI Style Guide](https://devcenter.heroku.com/articles/cli-style-guide)
