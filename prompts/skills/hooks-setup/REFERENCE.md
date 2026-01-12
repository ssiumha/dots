# hooks-setup Reference

## Hooks for Skills 개요

Claude Code v2.1.0+에서 추가된 기능으로, SKILL.md frontmatter에 hooks를 정의하여 해당 skill 사용 시에만 활성화됩니다.

## 글로벌 hooks vs 스킬 hooks

| 구분 | 글로벌 hooks | 스킬 hooks |
|------|-------------|-----------|
| 위치 | `.claude/settings.local.json` | SKILL.md frontmatter |
| 범위 | 전체 세션 | 해당 skill 사용 시만 |
| 설정 방법 | 워크플로우 1-3 | 워크플로우 4-8 (레시피) |

## 레시피 포맷

```yaml
name: recipe-name
description: 설명
event: Stop  # PreToolUse | PostToolUse | Stop
matcher: ""  # 도구명 정규식 (선택)
hooks:
  - type: command
    command: "${SKILL_DIR}/scripts/my-script.sh"

# 선택: 스크립트 템플릿
script_template: |
  #!/bin/bash
  echo "Hook executed"
```

## 변수

| 변수 | 설명 |
|------|------|
| `${SKILL_DIR}` | 적용 대상 skill 디렉토리 경로 |
| `${PROJECT_ROOT}` | 프로젝트 루트 (`.claude/` 상위) |

## 옵션

| 옵션 | 설명 |
|------|------|
| `once: true` | 세션당 1회만 실행 (초기 검증용) |
| `matcher` | 도구명 정규식 필터 (예: "Write\|Edit") |

## SKILL.md frontmatter 포맷

```yaml
---
name: skill-name
description: ...
hooks:
  Stop:
    - hooks:
        - type: command
          command: "path/to/script.sh"
  PostToolUse:
    - matcher: "Write|Edit"
      hooks:
        - type: command
          command: "path/to/another-script.sh"
          once: true  # 선택: 세션당 1회만
---
```

## 이벤트 종류

### PreToolUse
- **시점**: 도구 실행 전
- **용도**: 위험한 작업 차단
- **차단 방법**: stdout으로 JSON 출력 후 exit 2
  ```bash
  echo '{"decision": "block", "reason": "이유"}'
  exit 2
  ```

### PostToolUse
- **시점**: 도구 실행 완료 후
- **용도**: 즉각적 검증/포맷팅
- **주의**: 매 수정마다 실행 (빠른 도구만)

### Stop
- **시점**: skill 처리 완료 시
- **용도**: 전체 검증, 상태 저장, 로깅

## 레시피 목록

### auto-save-state
- **이벤트**: Stop
- **용도**: 스킬 완료 시 상태 파일에 타임스탬프 추가

### post-edit-lint
- **이벤트**: PostToolUse
- **matcher**: Write|Edit
- **용도**: 파일 수정 후 자동 lint (biome, ruff 등)

### post-edit-test
- **이벤트**: PostToolUse
- **matcher**: Write|Edit
- **용도**: 파일 수정 후 관련 테스트 실행

### pre-bash-guard
- **이벤트**: PreToolUse
- **matcher**: Bash
- **용도**: 위험한 명령어 차단 (rm -rf, --force 등)

### on-complete-log
- **이벤트**: Stop
- **용도**: 스킬 완료 로그 기록

## 트러블슈팅

| 문제 | 해결 |
|------|------|
| hook 미실행 | `/skills` 명령으로 인식 확인, frontmatter YAML 문법 검증 |
| 스크립트 오류 | 실행 권한 확인 (`chmod +x`), 경로 확인 |
| 변수 치환 안됨 | `${SKILL_DIR}` 대신 실제 경로로 직접 지정 |
| 롤백 필요 | `.bak` 파일에서 복원 또는 워크플로우 8 사용 |

## 참고 자료

- Claude Code Hooks 공식 문서: `claude --help hooks`
- Hooks for Skills는 Claude Code v2.1.0+에서 지원
