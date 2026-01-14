---
name: tidy
description: Performs small structural code cleanups (tidyings). Use when preparing code changes, removing dead code, reducing nesting, or cleaning up before feature work.
---

# Tidy

Kent Beck의 "Tidy First?" 방법론으로 작은 구조적 정리를 수행합니다.

## 핵심 철학

- **Tidying ≠ Refactoring**: 몇 분 내 완료되는 작은 정리
- **1 tidying = 1 commit**: 각 tidying은 반드시 별도 커밋
- **연속 실행 가능**: 여러 tidying 수행 시 여러 커밋 생성

> **WHY: 1 tidying = 1 commit**
> - 되돌리기 쉬운 단위
> - 각 변경을 개별 추적 가능
> - 문제 발생 시 특정 tidying만 revert

> **DECISION: 별도 커밋**
> - 이유: revert 용이, 리뷰 명확
> - 대안: 기능과 함께 커밋 (거부: 문제 원인 추적 어려움)

## 자동 트리거

| 조건 | 예시 |
|------|------|
| "tidy" + 작업 동사 | "코드 정리해줘", "tidy 해줘" |
| 기능 변경 전 정리 요청 | "이거 수정 전에 좀 정리하고 싶어" |
| 특정 패턴 언급 | "guard clause로 바꿔줘", "dead code 제거" |

## Instructions

### 워크플로우 1: 작업 영역 파악

1. **기준 커밋 확인** (tidy 커밋 제외)
   ```bash
   # tidy: 커밋을 제외한 최근 커밋
   git log --format='%H %s' --grep='^tidy:' --invert-grep -n 1
   ```
   이 커밋이 테스트 통과의 기준점이 됩니다.

2. **컨텍스트 확인**
   ```bash
   git status
   git diff --stat
   ```

3. **대상 파일 특정** (우선순위 순)
   1. 사용자가 명시한 파일
   2. git diff에 있는 수정 파일 → **전체 대상으로 자동 진행**
   3. git diff가 비어있고 명시 파일도 없는 경우에만 → AskUserQuestion

4. **파일 분석** (code-analysis.md 준수)
   - < 200줄: 전체 Read 허용
   - 200-500줄: ast-grep 또는 부분 Read
   - 500줄+: Explore subagent 필수

### 워크플로우 2: Tidying 후보 식별

대상 코드에서 적용 가능한 tidying 패턴 탐색:

| 우선순위 | 패턴 | 감지 신호 | 선택 이유 |
|----------|------|-----------|-----------|
| 1 | Guard Clause | 깊은 중첩, if-else 체인 | 중첩 감소로 버그 추적 용이 |
| 2 | Dead Code | 미사용 함수/변수, 주석 코드 | 혼란 제거, 오판 방지 |
| 3 | Explaining Variable | 복잡한 조건문, 매직 넘버 | 의도 명확화 |
| 4 | Normalize Symmetries | 비일관적 패턴 | 일관성 확보 (⚠️ 우연한 유사성 주의) |
| 5 | Reading Order | 함수 순서 불일치 | 흐름 개선 |
| 6 | Cohesion Order | 관련 코드 분산 | 응집도 향상 |

**Dead Code 탐지** (작업 파일 범위 내):
- 주석 처리된 코드 블록
- 파일 내 미호출 private 함수
- 미사용 로컬 변수
- 전체 프로젝트 스캔은 범위 초과 → 별도 작업으로

**상세 패턴**: `resources/01-tidying-patterns.md` 참조

**⚠️ 우연한 유사성 vs 진짜 중복**

| 구분 | 진짜 중복 | 우연한 유사성 |
|------|----------|--------------|
| 정의 | 동일 비즈니스 로직 | 구현 유사, 도메인 상이 |
| 특징 | 변경 시 함께 바뀜 | 독립 진화 가능 |
| 행동 | 통합 | 유지 |

**통합 금지 신호**: 다른 엔티티/도메인, 제네릭 필요, 조건문 증가

### 워크플로우 3: 제안 및 적용

1. **하나의 tidying 제안**
   ```
   ## Tidying 제안

   **패턴**: {패턴명}
   **위치**: {파일:라인}
   **변경**: {Before → After 요약}
   **이유**: {왜 이 정리가 도움이 되는지}
   ```

2. **사용자 승인 대기**
   - 승인 → 적용 → 커밋
   - 거절 → 다음 후보로
   - 수정 요청 → 조정 후 재제안

3. **적용 및 커밋**

   변경 적용 후 확인:
   ```
   커밋할까요?
   [1] Yes - "tidy: {패턴명} in {파일명}"
   [2] No - 다음 tidying으로
   [3] 메시지 수정
   ```

   승인 시:
   ```bash
   git add {파일}
   git commit -m "tidy: {패턴명} in {파일명}"
   ```

4. **다음 tidying으로** (후보가 남아있으면 반복)

### 워크플로우 4: 특정 패턴 적용

사용자가 특정 패턴을 요청한 경우:

```
User: "guard clause로 바꿔줘"
→ Guard Clause 패턴만 집중 탐색
→ 해당 패턴 적용
```

## 중요 원칙

1. **동작 변경 금지**: tidying은 구조만 변경, 동작은 그대로
2. **테스트 통과 유지**: 기준 커밋(tidy 제외 최근 커밋)과 동일한 테스트 결과 유지
   - 워크플로우 1에서 확인한 기준 커밋의 테스트 상태를 기준으로 함
   - export/public 함수 수정 시: 전체 테스트 (Task subagent)
   - 내부 로직만 변경 시: 해당 파일 테스트만
   - 테스트 없는 경우: WARNING 후 계속
   - 실패 시: 자동 `git restore {파일}` + 보고
3. **1 tidying = 1 commit**: 여러 tidying 실행 시 각각 별도 커밋
4. **되돌리기 쉽게**: 독립적인 커밋으로 개별 revert 가능

## Examples

### 기본 사용
```
User: "이 함수 정리해줘"
→ 워크플로우 1: 대상 파일 분석
→ 워크플로우 2: Guard Clause, Dead Code 발견
→ Guard Clause 제안 → 승인 → 커밋 #1
→ Dead Code 제안 → 승인 → 커밋 #2
→ "완료. 2개 tidying, 2개 커밋 생성됨"
```

### 특정 패턴 요청
```
User: "guard clause로 바꿔줘"
→ 워크플로우 4: Guard Clause 패턴만 탐색
→ 하나의 변환 제안 → 승인 → 커밋
→ 완료
```

### 일부 거절
```
User: "/tidy"
→ 3개 후보 발견
→ Guard Clause 제안 → 승인 → 커밋 #1
→ Dead Code 제안 → 거절 (skip)
→ Explaining Variable 제안 → 승인 → 커밋 #2
→ "완료. 2개 커밋 생성됨"
```

## Technical Details

### 커밋 메시지 형식

```
tidy: {패턴명} in {파일명}

- {구체적 변경 내용}
```

예시:
```
tidy: guard clause in auth.ts

- Convert nested if-else to early returns
- Reduce nesting depth from 4 to 1
```

### tidy-commit agent와의 관계

| tidy skill | tidy-commit agent |
|------------|-------------------|
| 코드 구조 정리 | 커밋 단위 정리 |
| 기능 변경 **전** | 작업 완료 **후** |
| 사용자 직접 호출 | proactive 호출 |

조합 사용:
```
/tidy → 코드 정리 → 기능 구현 → tidy-commit → 커밋 정리
```

### 리소스

- `resources/01-tidying-patterns.md`: 15가지 tidying 패턴 상세
