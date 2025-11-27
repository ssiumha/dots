# CLI 설계 철학

## 핵심 인용

> "모든 프로그램의 결과물은 다른 미지의 프로그램의 입력이 될 것으로 예상하라." — Doug McIlroy

> "표준이 생산성이나 사용자 만족도에 명백히 해롭다면 표준을 포기하라." — Jef Raskin

## 원칙

### 1. 인간 중심 설계

전통적으로 CLI는 다른 프로그램과의 상호작용을 중심으로 설계되었으나, 현대에는 **사람이 주 사용자**입니다.

- GUI 수준의 사용자 경험 추구
- 명확하고 친절한 피드백
- 오류 시 해결 방법 제안

### 2. 상호 연동성

작고 깔끔한 인터페이스를 가진 프로그램들이 조합되어 더 큰 시스템을 구축할 수 있어야 합니다.

```bash
# 좋은 예: 파이프로 조합 가능
cat file.txt | grep pattern | wc -l

# 나쁜 예: 모든 기능을 한 도구에
do-everything --grep pattern --count file.txt
```

**UNIX 관례 준수**:
- 표준 입출력 (stdin/stdout/stderr)
- 시그널 처리 (SIGINT, SIGTERM)
- 종료 코드 (0=성공, 1+=실패)

### 3. 일관성

터미널의 관례는 사용자의 습관으로 체화됩니다. 기존 패턴을 따를 때 직관성과 효율성이 높아집니다.

```bash
# 사용자가 기대하는 동작
-v, --verbose   # 상세 출력
-q, --quiet     # 조용한 모드
-f, --force     # 강제 실행
-h, --help      # 도움말
```

### 4. 적절한 정보량

"정보는 인터페이스"라는 관점에서 너무 많거나 너무 적은 출력 모두 피해야 합니다.

```bash
# 너무 적음
$ deploy
$

# 너무 많음
$ deploy
[DEBUG] Loading config...
[DEBUG] Parsing arguments...
[DEBUG] Connecting to server...
... (200줄)
Deployed!

# 적절함
$ deploy
Deploying to production...
Deployed! View at https://example.com
```

### 5. 발견 용이성

GUI의 이점(메뉴, 버튼)을 CLI에 적용합니다.

- 도움말 텍스트에 예제 포함
- 오류 시 수정 제안
- 다음 단계 명령 안내

```bash
$ git status
On branch main
Changes not staged for commit:
  modified:   file.txt

# 다음 단계 제안
use "git add <file>..." to update what will be committed
```

### 6. 대화형 상호작용

사용자가 명령어를 시행착오를 거쳐 실행하는 과정을 "대화"로 봅니다.

```bash
$ myapp pss
Unknown command: pss
Did you mean: ps?

$ myapp config --set foo
Missing required value for --set
Usage: myapp config --set KEY=VALUE
```

### 7. 견고성

기술적 견고함과 심리적 견고함을 모두 추구합니다.

**기술적**: 예외 처리, 타임아웃, 재시도
**심리적**: 즉각적 반응, 명확한 상태 표시, 진행률

```bash
$ long-task
Processing... [████████░░░░░░░░] 50%
```

### 8. 공감

사용자가 성공하기를 원한다는 의도를 행동으로 표현합니다.

```bash
# 나쁜 예
Error: Permission denied

# 좋은 예
Can't write to /etc/config.
This file is owned by root. Try: sudo myapp config
```

### 9. 창의적 규칙 위반

필요시 기존 관례를 의도적으로 깨뜨릴 수 있으나, 명확한 목적이 필요합니다.

예: `git`은 많은 서브커맨드를 도입하여 UNIX 전통을 깼지만, 복잡한 워크플로우를 단일 도구로 제공하는 가치가 있었습니다.

## 기본 규칙 체크리스트

- [ ] 명령줄 인자 파싱 라이브러리 사용 (docopt, Click, clap 등)
- [ ] 성공 시 exit 0 반환
- [ ] 실패 시 exit 1+ 반환
- [ ] 주요 출력은 stdout으로
- [ ] 메시지/오류/로그는 stderr로
