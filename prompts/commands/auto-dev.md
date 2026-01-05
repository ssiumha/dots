---
description: 통합 개발 워크플로우 (자율/수동 모드, 재개, 현황, 완료)
---

# Auto Dev

기능 요청부터 PR 생성까지 자율적으로 진행합니다.

## Instructions

이 커맨드는 `auto-dev` 스킬을 활성화합니다.
스킬이 활성화되면 CLAUDE.md의 "자율 개발 모드" 규칙이 적용됩니다.

## 사용법

| 옵션 | 설명 |
|------|------|
| `/auto-dev [기능]` | 자율 모드 - SPECIFY → PLAN → IMPLEMENT → COMPLETE → PR |
| `--manual` | 수동 모드 - 3문서 생성 후 사용자가 직접 진행 |
| `--continue` | 작업 재개 - state.md 유무에 따라 자율/수동 복원 |
| `--status` | 현황 파악 - 진행 중인 모든 작업 목록 |
| `--complete` | 수동 완료 - Living Docs 통합, 아카이브 (PR 생략) |
| `--stop` | 중단 - 현재 상태를 state.md에 저장 |

### 예시

```bash
# 자율 모드
/auto-dev 사용자 프로필에 다크모드 토글 추가
/auto-dev 로그인 API에 rate limiting 적용

# 수동 모드
/auto-dev --manual

# 재개
/auto-dev --continue

# 중단 (나중에 재개)
/auto-dev --stop
```

## 마이그레이션

기존 개별 명령은 auto-dev로 통합되었습니다 (기존 명령 파일 삭제됨).

| 기존 명령 | 통합 명령 |
|-----------|-----------|
| `/dev-start` | `/auto-dev --manual` |
| `/dev-continue` | `/auto-dev --continue` |
| `/dev-status` | `/auto-dev --status` |
| `/dev-complete` | `/auto-dev --complete` |

## 워크플로우

```
SPECIFY → PLAN → IMPLEMENT → COMPLETE
(요구사항)  (계획)   (구현)      (PR)
```

상세는 `auto-dev` 스킬 참조.
