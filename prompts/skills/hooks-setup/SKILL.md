---
name: hooks-setup
description: Configures Claude Code hooks for projects and skills. Use when setting up lint, test, or format hooks in .claude/ directory, or applying hook recipes to skills via SKILL.md frontmatter.
---

# Hooks Setup

프로젝트를 분석하여 적절한 Claude Code hooks를 `.claude/`에 설정합니다.
또한 **Hooks for Skills** (v2.1.0+)를 통해 skill별 hooks를 레시피로 관리합니다.

## Quick Reference

```
# 프로젝트 hooks
"hooks 설정해줘"              → 워크플로우 1-3

# 스킬 hooks (레시피)
"레시피 뭐 있어?"             → 워크플로우 4
"auto-dev에 레시피 적용해줘"  → 워크플로우 5
"새 레시피 만들어"            → 워크플로우 7
"hooks 제거해줘"              → 워크플로우 8
```

**두 가지 모드**:
- **프로젝트 hooks**: `.claude/settings.local.json` (워크플로우 1-3)
- **스킬 hooks**: SKILL.md frontmatter (워크플로우 4-8)

## 자동 트리거

| 조건 | 예시 |
|------|------|
| hooks 설정 요청 | "hooks 설정해줘", "lint hook 추가" |
| 프로젝트 초기화 시 | claude-project-setup에서 호출 |
| 레시피 관련 요청 | "레시피 뭐 있어?", "hooks 레시피" |
| skill에 레시피 적용 | "auto-dev에 auto-save-state 적용" |

## Instructions

### 워크플로우 1: 프로젝트 분석

1. **프로젝트 타입 감지**

   ```bash
   ls -la package.json pyproject.toml Cargo.toml go.mod Gemfile 2>/dev/null
   ```

   | 파일 | 언어 | 기본 도구 |
   |------|------|----------|
   | `package.json` | TypeScript/JS | biome, eslint, prettier |
   | `pyproject.toml` | Python | ruff, black, pytest |
   | `Cargo.toml` | Rust | rustfmt, clippy, cargo test |
   | `go.mod` | Go | gofmt, golangci-lint, go test |
   | `Gemfile` | Ruby | rubocop, rspec |

2. **기존 설정 확인**

   ```bash
   ls -la biome.json .eslintrc* ruff.toml .rubocop.yml 2>/dev/null
   ```

3. **사용자 확인**

   AskUserQuestion으로 확인:
   - 감지된 프로젝트 타입 확인
   - 설정할 hook 종류 선택 (lint, test, format)

### 워크플로우 2: Hook 설정 생성

1. **디렉토리 생성**

   ```bash
   mkdir -p .claude/hooks
   ```

2. **Hook 스크립트 생성**

   `.claude/hooks/post-edit.sh`:
   ```bash
   #!/usr/bin/env bash
   # PostToolUse hook for Edit/Write

   set -euo pipefail

   # 입력에서 파일 경로 추출
   INPUT=$(cat)
   FILE_PATH=$(echo "$INPUT" | jq -r '.tool_input.file_path // empty')

   [[ -z "$FILE_PATH" ]] && exit 0
   [[ ! -f "$FILE_PATH" ]] && exit 0

   # 확장자별 linter 실행 (최소 로깅, 실패 허용)
   case "$FILE_PATH" in
     *.ts|*.tsx|*.js|*.jsx)
       if command -v npx &>/dev/null; then
         # biome 우선, 없으면 eslint 시도
         if [[ -f biome.json ]]; then
           npx biome check --write "$FILE_PATH" 2>&1 | head -3 >&2 || true
         elif ls .eslintrc* &>/dev/null 2>&1; then
           npx eslint --fix "$FILE_PATH" 2>&1 | head -3 >&2 || true
         fi
       fi
       ;;
     *.py)
       if command -v ruff &>/dev/null; then
         ruff check --fix "$FILE_PATH" 2>&1 | head -3 >&2 || true
       fi
       ;;
     *.rb)
       if command -v rubocop &>/dev/null; then
         rubocop -A "$FILE_PATH" 2>&1 | head -3 >&2 || true
       fi
       ;;
     *.go)
       if command -v gofmt &>/dev/null; then
         gofmt -w "$FILE_PATH" 2>&1 | head -3 >&2 || true
       fi
       ;;
   esac

   exit 0  # hook 실패가 작업을 중단시키지 않음
   ```

3. **settings.local.json 생성**

   `.claude/settings.local.json`:
   ```json
   {
     "hooks": {
       "PostToolUse": [
         {
           "matcher": "Edit|Write",
           "hooks": [
             {
               "type": "command",
               "command": ".claude/hooks/post-edit.sh"
             }
           ]
         }
       ]
     }
   }
   ```

4. **실행 권한 부여**

   ```bash
   chmod +x .claude/hooks/*.sh
   ```

### 워크플로우 3: 프로젝트별 커스터마이징

사용자 요청에 따라 hook 수정:

**Lint만 (포맷팅 제외)**:
```bash
# --write 대신 --check만
npx biome check "$FILE_PATH"
```

**Test 자동 실행 (Stop hook)**:
```json
{
  "hooks": {
    "Stop": [{
      "hooks": [{
        "type": "command",
        "command": ".claude/hooks/run-tests.sh"
      }]
    }]
  }
}
```

**특정 파일만 제외**:
```bash
# 생성된 파일, 벤더 파일 제외
[[ "$FILE_PATH" == *"/vendor/"* ]] && exit 0
[[ "$FILE_PATH" == *".generated."* ]] && exit 0
```

## Hook 종류별 가이드

### PostToolUse (권장)

**용도**: 파일 수정 직후 즉각적 검증/포맷팅

```json
{
  "matcher": "Edit|Write",
  "hooks": [{ "type": "command", "command": ".claude/hooks/post-edit.sh" }]
}
```

**장점**: 즉각 피드백, 파일 단위 처리
**단점**: 매 수정마다 실행 (느린 linter 주의)

### Stop (선택)

**용도**: 작업 완료 후 전체 검증

```json
{
  "matcher": "",
  "hooks": [{ "type": "command", "command": ".claude/hooks/on-stop.sh" }]
}
```

**적합한 경우**:
- 전체 테스트 실행
- 빌드 검증
- 타입 체크 (`tsc --noEmit`)

### PreToolUse (주의)

**용도**: 위험한 작업 차단

```json
{
  "matcher": "Bash",
  "hooks": [{ "type": "command", "command": ".claude/hooks/pre-bash.sh" }]
}
```

**적합한 경우**:
- `rm -rf` 차단
- 프로덕션 파일 보호
- 민감 명령어 검증

---

## Hooks for Skills (레시피 관리)

### 워크플로우 4: 레시피 탐색

**트리거**: "레시피 뭐 있어?", "hooks 레시피", "--recipes"

1. **레시피 목록 출력**
   ```bash
   bash scripts/list-recipes.sh
   ```

2. **사용 가능한 레시피**

   | 레시피 | 이벤트 | 용도 |
   |--------|--------|------|
   | `auto-save-state` | Stop | 스킬 완료 시 상태 저장 |
   | `post-edit-lint` | PostToolUse | 파일 수정 후 lint |
   | `post-edit-test` | PostToolUse | 파일 수정 후 테스트 |
   | `pre-bash-guard` | PreToolUse | 위험 명령 차단 |
   | `on-complete-log` | Stop | 완료 로그 기록 |

3. **상세 보기**: 사용자가 특정 레시피 선택 시 YAML 내용 표시

### 워크플로우 5: 레시피 적용

**트리거**: "auto-dev에 auto-save-state 적용", "--skill X --recipe Y"

1. **대상 skill 확인**
   - `prompts/skills/{target}/SKILL.md` 존재 확인
   - 없으면 에러 메시지

2. **레시피 로드**
   - `recipes/{recipe-name}.yaml` Read

3. **백업 생성**
   ```bash
   cp SKILL.md SKILL.md.bak
   ```

4. **변수 치환**
   - `${SKILL_DIR}` → 실제 skill 경로
   - `${PROJECT_ROOT}` → 프로젝트 루트

5. **frontmatter 수정**
   ```yaml
   ---
   name: target-skill
   description: ...
   hooks:
     Stop:
       - hooks:
           - type: command
             command: "prompts/skills/target-skill/scripts/save-state.sh"
   ---
   ```

6. **스크립트 생성** (레시피에 script_template 있으면)
   - 스크립트 파일 생성
   - `chmod +x` 실행

7. **검증**: YAML 문법 확인

### 워크플로우 6: 레시피 조합

**트리거**: "여러 레시피 적용", "--recipe a,b,c"

1. 레시피 목록 파싱
2. 각 레시피 순차 적용
3. 동일 event는 hooks 배열에 병합
4. 중복 제거 (동일 matcher + command)

### 워크플로우 7: 레시피 생성

**트리거**: "새 레시피 만들어", "--new-recipe"

1. **이름 결정** (kebab-case)
2. **이벤트 선택**: PreToolUse / PostToolUse / Stop
3. **matcher 설정**: 도구명 정규식 (예: "Write|Edit")
4. **command 설정**: 실행할 스크립트 경로
5. **템플릿 복사**
   ```bash
   cp templates/recipe-template.yaml recipes/{name}.yaml
   ```
6. **내용 수정**

### 워크플로우 8: hooks 제거

**트리거**: "hooks 제거", "--remove-hooks", "--rollback"

1. **대상 skill 확인**
2. **롤백 옵션**: `.bak` 파일 있으면 복원 가능
3. **frontmatter에서 hooks 섹션 삭제**
4. **관련 스크립트 삭제 여부 질문**

---

## 중요 원칙

1. **프로젝트 우선**: 글로벌 설정보다 `.claude/` 설정 우선
2. **기존 도구 활용**: 프로젝트에 이미 있는 linter 설정 사용
3. **실패 허용**: hook 실패가 작업을 중단시키지 않도록 (`|| true`)
4. **빠른 실행**: PostToolUse는 빠른 도구만 (느린 건 Stop으로)
5. **글로벌 vs 스킬별 분리**: 프로젝트 전체는 settings.local.json, 스킬 특화는 frontmatter
6. **레시피 재사용**: 한번 정의한 패턴을 여러 skill에 적용
7. **비파괴적 적용**: 레시피 적용 전 백업 생성 (.bak)

## Examples

### TypeScript 프로젝트
```
User: "이 프로젝트에 hooks 설정해줘"
→ package.json 감지
→ biome.json 확인
→ PostToolUse lint hook 생성
→ .claude/settings.local.json 생성
```

### Python 프로젝트 + 테스트
```
User: "lint랑 test hook 둘 다 설정해줘"
→ pyproject.toml 감지
→ ruff, pytest 확인
→ PostToolUse (ruff) + Stop (pytest) hook 생성
```

### 레시피 목록 확인
```
User: "hooks 레시피 뭐 있어?"
→ 워크플로우 4: 레시피 탐색
→ 5개 레시피 목록 출력
```

### 레시피 적용
```
User: "auto-dev에 auto-save-state 적용해줘"
→ 워크플로우 5: 레시피 적용
→ auto-dev/SKILL.md.bak 백업
→ frontmatter에 hooks 추가
→ scripts/save-state.sh 생성
```

### 다중 레시피 적용
```
User: "tdd-practices에 post-edit-test랑 on-complete-log 같이 적용"
→ 워크플로우 6: 레시피 조합
→ PostToolUse + Stop hooks 병합
```

## 안티패턴

| ❌ 문제 | ✅ 해결 |
|--------|--------|
| 레시피에 절대 경로 하드코딩 | `${SKILL_DIR}` 변수 사용 |
| 모든 skill에 동일 hooks 수동 추가 | 레시피로 일괄 적용 |
| 글로벌 hooks로 스킬 특화 로직 | frontmatter hooks 사용 |
| 스크립트 없이 command만 설정 | script_template으로 자동 생성 |
| 백업 없이 frontmatter 수정 | .bak 파일 먼저 생성 |

## Technical Details

### 스크립트
```bash
# 레시피 목록 출력
bash scripts/list-recipes.sh
```

### 리소스
- `REFERENCE.md`: Hooks for Skills 상세 문서
- `recipes/`: 레시피 YAML 파일들
- `templates/recipe-template.yaml`: 새 레시피 작성용

## 참고

- Claude Code Hooks 공식 문서: `claude --help hooks`
- Hooks for Skills는 Claude Code v2.1.0+에서 지원
