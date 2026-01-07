# 핵심 프롬프트

- **IMPORTANT**: 지시 내용과 관한 프로젝트 내의 관련 문서와 코드를 충분히 검토하기 전에는 어떤 작업도 시작하지 마십시오
- 복잡한 구현 요청 시 **Research First**: 코드 작성 전 관련 파일 조사 → 계획 수립 → 구현
- 사용자가 수동 접근을 언급하면, Claude가 자동화할 수 있는지 제안하십시오
- **같은 동작이라면 간결한 설명이 더 좋습니다** - 장황한 설명보다 핵심만 전달하십시오

# Anti-patterns (학습된 실수)

- 테스트 없이 "동작 확인됨"이라고 말하지 마십시오
- 파일을 읽기 전에 내용을 추측하지 마십시오
- 에러 발생 시 원인 분석 없이 바로 수정하지 마십시오
- 기존 코드 제거/단순화 전에 "왜 이렇게 작성되었는지" 파악하십시오 (Chesterton's Fence)
- 페이즈/논리적 단위마다 커밋하십시오 (conventional commits: feat/fix/refactor/docs/test/chore)
- 사용자 확인 없이 범위를 확장하지 마십시오

# Context Engineering

Context는 유한한 자원입니다. 최소한의 고품질 토큰으로 최대 효과를 추구하십시오.

- **외부 메모리 활용**: 모든 것을 기억시키지 말고, 파일에 저장 후 필요 시 Read
- **서브에이전트 요약**: 전문 작업 후 핵심만 반환 (1-2K 토큰)
- **반복 정보 분리**: 자주 참조하는 정보는 전용 파일로 분리
- **Subagent 조사 자동 활용**: 여러 파일/모듈 탐색 시 subagent로 병렬 조사 (컨텍스트 절약)

# 모델 선택 전략

- **탐색/검색**: haiku (빠름, 저비용)
- **일반 작업**: sonnet (균형)
- **복잡한 설계/분석**: opus with thinking (정밀, 조정 최소화로 결과적으로 빠름)

# 반복 작업 자동화

동일한 패턴의 작업이 2회 이상 반복되면, 자동화를 제안하십시오:

## 스크립트 제안
- **탐색 작업**: 매번 Glob/Grep 대신 → 전용 검색 스크립트
- **검증 작업**: 매번 수동 확인 대신 → 자동화된 체크 스크립트
- **변환 작업**: 매번 수동 처리 대신 → 일괄 변환 스크립트

## 훅 제안
다음 패턴 감지 시 PostToolUse 훅을 제안하십시오:
- Write/Edit 후 매번 포매터 실행 → `.claude/hooks/` 훅으로 자동화
- 특정 파일 수정 후 항상 같은 검증 → 훅으로 자동화

스크립트 위치: `bin/`, `scripts/`, 또는 프로젝트 관례에 따름
훅 위치: `.claude/hooks/`

# 병렬 실행

## 원칙

**판단 기준**: 작업 간 의존성(import, 실행 순서, 공유 상태)이 없으면 병렬 처리

| 병렬 가능 | 순차 필요 |
|----------|----------|
| 다른 모듈/디렉토리에 동일 작업 | 한 작업 결과가 다음 입력 |
| 독립적인 파일 생성/수정 | 같은 파일 수정 |
| 읽기 전용 탐색/검색 | 빌드 → 테스트 → 배포 |

## 실행 방법

| 방법 | 조건 | 비고 |
|------|------|------|
| 터미널/탭 분리 | 독립 작업 2+ | `&`, `--teleport` |
| Subagent 병렬 | 독립 작업 3+ | 최대 10개, 토큰 3-4배 |
| 백그라운드 종단 | 작업 완료 후 | `run_in_background=true` |

종단 작업(문서 동기화, 코드 리뷰 등)은 subagent description 참조.

# Subagent 적극 활용

## Proactive 호출
조건 충족 시 **즉시** 호출 (사용자 요청 불필요):
- 2+ 파일 수정 → code-reviewer (백그라운드)
- .ts/.tsx 수정 → code-review-typescript
- 작업 완료 → ldoc-automation
- 페이즈 완료/작업 종료 → tidy-commit (커밋 정리)

## 동적 subagent 생성
복잡한 작업은 즉석에서 전문 subagent 정의 후 실행:

```
Task(subagent_type="general-purpose", prompt="You are an expert {domain}. ...")
```

적용 시점:
- 탐색 범위가 넓을 때 (3+ 파일/모듈)
- 전문 분석 필요 (보안, 성능, 아키텍처)
- 반복 작업 병렬화

## 영구 subagent 제안
동일 패턴 3회+ 반복 시 → `agent-creator`로 영구 subagent 생성 제안

# 금지 사항

- **무슨 일이 있어도 절대로 DO NOT MUST --no-verify, --force 같은 옵션을 사용하지 마십시오**
- 일회성 디버깅/수정 스크립트를 작성하지 마십시오
  - ex) debug-not-working-feature.sh, fix-permissions.js 등
  - 단, 반복 패턴 자동화 스크립트는 제안 가능 (위 "반복 작업 자동화" 참조)

# 자율 개발 모드

`/auto-dev` 참조. 핵심: SPECIFY 필수 → 테스트 통과 + PR까지 자율 진행.

# Skill ↔ Subagent 연동

스킬 워크플로우를 subagent로 자동화: `prompts/agents/{skill}-automation.md`

호출: `Task(subagent_type="{skill}-automation", prompt="{워크플로우}: {컨텍스트}")`

상세: `agent-creator` skill 참조.
