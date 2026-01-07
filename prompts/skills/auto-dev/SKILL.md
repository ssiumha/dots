---
name: auto-dev
description: 기능 구현을 PR까지 자율적으로 진행할 때 사용. 복잡한 작업, 멀티 세션 작업에서 컨텍스트를 유지하며 테스트까지 완료합니다. (user)
---

# Auto Dev

기능 요청부터 PR 생성까지 자율적으로 진행하는 오케스트레이터 스킬.

## Quick Reference

```
/auto-dev [기능]      # 자율 모드 (SPECIFY → PR)
/auto-dev --manual    # 수동 모드 (3문서 생성)
/auto-dev --continue  # 작업 재개
/auto-dev --status    # 현황 파악
/auto-dev --complete  # 수동 완료
/auto-dev --stop      # 상태 저장 후 종료
```

## 핵심 철학

1. **요구사항 확정 우선**: 구현 전 모든 모호함 해소
2. **멈추지 않음**: 요구사항 확정 후 완료 조건까지 자율 진행
3. **작은 커밋**: task 단위로 커밋하여 롤백 가능
4. **세션 복원**: state.md로 컨텍스트 유지

## Instructions

### Phase 1: SPECIFY (요구사항 확정)

**목표**: 구현 전 모든 모호함 해소

1. **spec-validator agent 실행**
   - Task tool로 호출: `subagent_type: spec-validator`
   - 입력: 사용자의 기능 요청 원문
   - 출력: 질문 목록 + 완료 조건 초안

2. **사용자 확인**
   - spec-validator가 생성한 질문에 답변
   - 완료 조건 합의

3. **Phase 전환 조건**
   - 요구사항이 명확히 확정됨
   - 완료 조건(테스트 목록)이 합의됨

### Phase 2: PLAN (계획 수립)

**목표**: 구현 계획 수립 및 문서화

1. **dev-docs 워크플로우 1 실행**
   - 작업 디렉토리 생성
   - plan.md, context.md, tasks.md 생성

2. **Living Docs 관련 지식 확인**
   - 기존 관련 문서 검색 (ldoc 워크플로우 5)
   - 재사용 가능한 패턴 확인

3. **Tasks 분해**
   - 구현 단계를 tasks.md에 체크리스트로 작성
   - 단순 → 복잡 순서로 정렬
   - 각 task는 독립적으로 테스트 가능하게

4. **state.md 생성**
   - 현재 phase: PLAN
   - 완료 조건 목록
   - retry_count: 0

5. **Phase 전환 조건**
   - tasks.md에 task가 1개 이상
   - state.md 생성 완료

### Phase 3: IMPLEMENT (구현 루프)

**목표**: TDD로 각 task 구현

```
LOOP until 모든 task 완료 (최대 20회 반복, 10회 초과 시 사용자 확인):
  1. tasks.md에서 다음 미완료 task 선택
  2. tdd-practices 적용:
     - RED: 실패하는 테스트 작성
     - GREEN: 최소 구현으로 통과
     - REFACTOR: 정리
  3. 테스트 실행
  4. [실패] → 막힘 대응 (아래 참조)
  5. [성공] → test-verifier agent로 품질 검증
     - Task tool로 호출: `subagent_type: test-verifier`
     - 입력: 테스트 파일 경로, 실행 결과, 완료 조건
     - [High/Medium 이슈] → 수정 후 재검증 (task당 최대 2회)
     - [3회 초과] → 블로커로 전환
     - [Low 이슈만 or 없음] → 진행
  6. 커밋, tasks.md 체크 표시
  7. state.md 업데이트
```

**막힘 대응 (동일 에러 기준)**:
- 1차: 에러 메시지 분석, 관련 코드 검토
- 2차: 다른 접근법 시도
- 3차: 최소 범위로 축소 테스트
- 4차: state.md에 블로커 기록 → 사용자 질문

**Phase 전환 조건**:
- tasks.md의 모든 checkbox가 [x]
- 전체 테스트 통과

### Phase 3.5: VERIFY (검증 루프)

**목표**: 품질 2-3배 향상을 위한 체계적 검증

모든 task 완료 후, PR 생성 전 필수 실행:

```
VERIFY_LOOP (최대 3회):
  1. 전체 테스트 실행
  2. [실패] → 수정 후 다시 1번
  3. code-reviewer agent 호출 (Task tool)
     - 입력: 변경된 파일 목록
     - 분석: 품질, 보안, 성능
  4. [Critical/High 이슈] → 수정 후 다시 1번
  5. [Medium 이하만] → VERIFY 완료
```

**조기 종료 조건**:
- 테스트 통과 + 이슈 없음 → 1회차에서 종료
- 3회 반복 후에도 Critical 이슈 → 블로커로 전환

### Phase 4: COMPLETE (완료)

**목표**: 마무리 및 PR 생성

1. **VERIFY 완료 확인**
   - Phase 3.5 검증 루프 통과 필수
   - 미통과 시 Phase 3.5로 복귀

2. **dev-docs 워크플로우 4 실행**
   - Living Docs 통합 제안
   - 아카이브 이동

3. **PR 생성**
   ```bash
   gh pr create --title "feat: {기능명}" --body "$(cat <<'EOF'
   ## Summary
   - {구현 내용 요약}

   ## Changes
   - {변경 파일 목록}

   ## Test
   - {테스트 통과 여부}

   ---
   🤖 Generated with auto-dev skill
   EOF
   )"
   ```

4. **완료 보고**
   - PR URL 제공
   - 구현 요약

## 막힘 대응 매트릭스

| 상황 | 1차 시도 | 2차 시도 | 3차 시도 | 이후 |
|------|----------|----------|----------|------|
| 테스트 실패 | 에러 분석 | 코드 재검토 | 다른 접근 | 질문 |
| 타입 에러 | 타입 정의 확인 | 의존성 확인 | 캐스팅 시도 | 질문 |
| 빌드 실패 | 로그 분석 | 의존성 확인 | 캐시 정리 | 질문 |

**retry_count 관리**:
- 동일 에러 해시로 판단: `에러타입:파일:라인` 조합 (예: `TypeError:auth.py:42`)
- 3회 초과 시 블로커로 전환
- 다른 에러면 카운트 리셋

## 스킬/에이전트 연동

| Phase | 호출 | 역할 |
|-------|------|------|
| SPECIFY | spec-validator agent | 요구사항 검증, 질문 생성 |
| PLAN | dev-docs WF1 | 3문서 생성 |
| PLAN | ldoc 워크플로우 5 | 관련 지식 검색 |
| IMPLEMENT | tdd-practices | RED-GREEN-REFACTOR |
| IMPLEMENT | test-verifier agent | Overfitting 감지, 테스트 품질 검증 |
| VERIFY | code-reviewer agent | 품질, 보안, 성능 분석 |
| COMPLETE | dev-docs WF4 | Living Docs 통합 |

## 파일 위치

```
~/docs/dev/{project}/active/{task-name}/
├── plan.md      # auto-dev가 dev-docs WF1 호출하여 생성
├── context.md   # auto-dev가 dev-docs WF1 호출하여 생성
├── tasks.md     # auto-dev가 dev-docs WF1 호출하여 생성
└── state.md     # auto-dev가 직접 생성 (templates/state.md 기반)
```

**템플릿**: `prompts/skills/auto-dev/templates/state.md`

## 옵션별 동작

### (기본) 자율 모드

`/auto-dev [기능 설명]`

4단계 워크플로우 자동 진행: SPECIFY → PLAN → IMPLEMENT → COMPLETE

### --manual (수동 모드)

`/auto-dev --manual`

dev-docs 워크플로우 1 (작업 시작) 실행:
1. 작업명 입력 받기 (kebab-case)
2. Living Docs 관련 문서 확인 (선택)
3. 작업 디렉토리 생성
4. 3개 문서 생성 (plan.md, context.md, tasks.md)
5. 사용자 확인

**자율 모드와 차이점**: state.md 생성 안 함, 사용자가 직접 진행

### --continue (재개)

`/auto-dev --continue`

**Case 1: state.md 있음 (auto-dev 세션)**
1. `~/docs/dev/{project}/active/` 검색
2. state.md가 있는 작업 찾기
3. phase와 진행 상황 복원
4. 자율 모드로 해당 phase부터 재개

**Case 2: state.md 없음 (일반 dev-docs 세션)**
1. `~/docs/dev/{project}/active/` 검색
2. 3문서(plan.md, context.md, tasks.md) 읽기
3. 사용자에게 요약 제공
4. 자율 모드 활성화 여부 질문:
   ```
   자율 모드로 진행할까요?
   [1] 예 - PR까지 자율 진행
   [2] 아니오 - 수동 진행
   ```

**Case 3: 작업 없음**
```
📭 진행 중인 작업이 없습니다.
새 작업을 시작하려면: /auto-dev [기능 설명]
```

**Case 4: state.md 손상**
- state.md 파싱 실패 시 3문서 기반으로 복원
- 복원 후 사용자에게 확인 요청

### --status (현황 파악)

`/auto-dev --status`

dev-docs 워크플로우 3 (현황 파악) 실행:
1. 프로젝트 확인
2. Active 작업 검색 (`~/docs/dev/{project}/active/*/plan.md`)
3. 각 작업의 frontmatter 분석 (status, created, updated)
4. 표 형식으로 리포트:
   ```
   📋 진행 중인 작업 (N개)

   | 작업명 | 상태 | 생성일 | 마지막 수정 |
   |--------|------|--------|-------------|
   | feature-user-auth | in-progress | 2025-01-15 | 2025-01-20 |
   ```

### --complete (수동 완료)

`/auto-dev --complete`

dev-docs 워크플로우 4 (작업 완료) 실행:
1. 작업 확인 (현재 작업 또는 선택)
2. 3개 문서 읽기
3. plan.md 완료 처리 (status: completed)
4. Context 분석 및 Living Docs 통합 제안
5. 아카이브 이동

**자율 모드와 차이점**: PR 생성 생략, Living Docs 통합 제안

### --stop (중단)

`/auto-dev --stop`

1. 현재 상태를 state.md에 기록
2. 진행 중인 task 표시
3. 안전하게 종료

**SPECIFY phase에서 중단 시:**
- 요구사항 확정 내역을 state.md에 기록
- 다음 재개 시 남은 질문부터 진행

**아직 작업 디렉토리 없을 때:**
- 임시 state만 메모리에 유지
- 다음 세션에서 수동으로 재시작 필요

## 자율 모드 vs 수동 모드

| 항목 | 자율 모드 | 수동 모드 (--manual) |
|------|----------|---------------------|
| 시작 | `/auto-dev [기능]` | `/auto-dev --manual` |
| SPECIFY | 자동 (질문 → 확정) | 생략 |
| state.md | 생성됨 | 생성 안 됨 |
| Phase 전환 | 자동 | 사용자가 직접 |
| PR 생성 | 자동 | 생략 |
| 적합한 경우 | 명확한 기능 개발 | 탐색적 작업, 부분 구현 |

## 중요 원칙

1. **요구사항 확정 전 구현 금지**: SPECIFY 완료 전까지 코드 작성 안 함
2. **작은 단위**: task 하나씩, 커밋 하나씩
3. **테스트 필수**: 테스트 없이 구현 없음
4. **무한 루프 방지**: 동일 에러 3회까지만
5. **세션 복원**: state.md 항상 최신 유지
6. **Context 압축**: 긴 세션 시 핵심만 state.md에 요약

## Context 압축 (긴 세션용)

세션이 길어지면 state.md에 압축된 컨텍스트 유지:

```markdown
## 압축된 컨텍스트 (마지막 업데이트: {timestamp})

### 완료된 작업
- task1: 미들웨어 구현 완료
- task2: 테스트 통과

### 현재 상태
- 진행 중: task3 (DB 연동)
- 마지막 에러: ConnectionError (1/3 재시도)

### 핵심 결정사항
- Redis 대신 인메모리 캐시 선택 (이유: 단순성)
```

**업데이트 시점**:
- 각 task 완료 시
- Phase 전환 시
- 에러 발생 시

## 진행 상황 표시

각 Phase 전환 및 주요 작업 시 사용자에게 표시:

```
📋 [SPECIFY] 요구사항 확인 중...
✅ [SPECIFY] 완료 - 3개 요구사항 확정

📋 [PLAN] 계획 수립 중...
✅ [PLAN] 완료 - 5개 task 생성

📋 [IMPLEMENT] 구현 중... (2/5)
⚠️ [IMPLEMENT] 에러 발생 - 재시도 1/3
✅ [IMPLEMENT] task 완료 - "미들웨어 구현"

📋 [COMPLETE] 마무리 중...
✅ [COMPLETE] PR 생성: https://github.com/.../pull/42
```

## 다중 프로젝트 처리

여러 프로젝트에서 작업 중일 때:

1. **프로젝트 결정**: 현재 디렉토리의 git root로 판단
2. **작업 검색**: `~/docs/dev/{project}/active/` 에서만 검색
3. **프로젝트 전환 시**: 다른 git repo로 이동하면 자동으로 해당 프로젝트 작업 표시

```
📁 현재 프로젝트: myapp
📋 진행 중인 작업: feature-rate-limiter

다른 프로젝트 작업은 해당 디렉토리에서 /auto-dev --status
```

## AI Red Flags

다음 패턴 감지 시 **즉시 중단**하고 사용자에게 알림:

| 패턴 | 감지 방법 | 대응 |
|------|----------|------|
| SPECIFY 생략 | 질문 없이 코드 작성 시작 | 중단 → SPECIFY로 복귀 |
| 테스트 없는 커밋 | 테스트 실행 없이 git commit | 커밋 거부 → 테스트 요청 |
| 무한 재시도 | retry_count > 3 | 중단 → 블로커 기록 → 질문 |
| 범위 확장 | tasks.md에 없는 작업 감지 | 중단 → 범위 확인 질문 |
| state.md 미갱신 | phase 전환 후 state 불일치 | 자동 복구 시도 → 알림 |

## Examples

### Example 1: 자율 모드 전체 흐름

```
User: /auto-dev 로그인 API에 rate limiting 추가

=== SPECIFY ===
📋 [SPECIFY] 요구사항 확인 중...
Assistant: 확인이 필요합니다:
1. 제한 기준: IP당? 사용자당?
2. 제한값: 분당 몇 회?
3. 초과 시 응답: 429? 지연?

User: IP당, 분당 60회, 429 응답
✅ [SPECIFY] 완료 - 3개 요구사항 확정

=== PLAN ===
📋 [PLAN] 계획 수립 중...
(dev-docs WF1 → plan.md, context.md, tasks.md 생성)
(state.md 생성)
✅ [PLAN] 완료 - 4개 task 생성

=== IMPLEMENT ===
📋 [IMPLEMENT] 구현 중... (1/4)
(TDD 루프 반복)
✅ [IMPLEMENT] task 완료 - "미들웨어 구현"

=== COMPLETE ===
📋 [COMPLETE] 마무리 중...
✅ [COMPLETE] PR 생성: https://github.com/.../pull/42
```

### Example 2: 세션 재개 (--continue)

```
User: /auto-dev --continue

# Case 1: state.md 있음
Assistant: (state.md 읽기 → Phase 3 IMPLEMENT 재개)
"📋 마지막 작업: rate limiter 구현 중 (2/4 완료)
자율 모드로 재개합니다..."

# Case 2: state.md 없음, 작업 1개
Assistant: (3문서 읽기 → 요약 제공)
"🔄 작업을 이어갑니다: feature-rate-limiter
자율 모드로 진행할까요?
[1] 예 - PR까지 자율 진행
[2] 아니오 - 수동 진행"

# Case 3: 작업 여러 개
Assistant:
"진행 중인 작업이 2개 있습니다:
1. feature-rate-limiter (in-progress)
2. bugfix-login-timeout (blocked)
어떤 작업을 이어서 하시겠습니까?"
```

### Example 3: 현황 파악 (--status)

```
User: /auto-dev --status

📋 진행 중인 작업 (2개)

| 작업명 | 상태 | 생성일 | 마지막 수정 |
|--------|------|--------|-------------|
| feature-rate-limiter | in-progress | 2025-01-15 | 2025-01-20 |
| bugfix-login-timeout | blocked | 2025-01-18 | 2025-01-19 |

💡 작업을 이어서 하려면: /auto-dev --continue
```

### Example 4: 수동 완료 (--complete)

```
User: /auto-dev --complete

📊 Living Docs로 영속화할 내용:
📋 의사결정 (1개): Redis 기반 rate limit 선택
🏗️ 아키텍처 (1개): 미들웨어 패턴

[1] 모두 Living Docs에 기록 (권장)
[2] 선택적으로 기록
[3] 아카이브만

User: 1
✅ 작업 완료! 아카이브: ~/docs/dev/myapp/archive/feature-rate-limiter/
```

### Example 5: 수동 모드 (--manual)

```
User: /auto-dev --manual

작업명을 정해주세요 (kebab-case):
User: feature-user-profile

✅ 작업을 시작합니다: feature-user-profile
📁 위치: ~/docs/dev/myapp/active/feature-user-profile/
   - plan.md, context.md, tasks.md 생성 완료

💡 이후 진행은 사용자가 직접 합니다 (자율 모드 아님)
```