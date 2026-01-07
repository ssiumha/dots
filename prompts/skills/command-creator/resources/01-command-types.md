# Command 유형 상세

## 1. Simple

가장 기본적인 형태. 단일 작업을 수행합니다.

**적합한 경우**:
- 단일 도구 실행 (lint, format, build)
- 간단한 조회 (status, info)
- 파일 생성 (scaffold)

**구조**:
```markdown
# Command Name
설명
$ARGUMENTS: 인자
## Steps
1. ...
2. ...
```

**예시**: `/format`, `/lint`, `/build`

---

## 2. Batch Processor

체크리스트 기반 병렬 처리. 대량 작업에 적합합니다.

**적합한 경우**:
- 여러 파일/항목 처리
- agent 병렬 호출
- 진행 상태 추적 필요

**핵심 요소**:
- 체크리스트 파일 (YAML/JSON)
- `done: true/false` 상태 관리
- Task tool로 병렬 agent 호출
- 배치 크기 제한 (권장: 5-10개)

**예시**: `/img-batch`, `/migrate`, `/refactor-batch`

---

## 3. Interactive

사용자 입력 기반 동적 실행. 설정/초기화에 적합합니다.

**적합한 경우**:
- 초기 설정 (init, setup)
- 선택지 기반 분기
- 사용자 확인 필요

**핵심 요소**:
- AskUserQuestion 활용
- 선택지 제공 ([1], [2], [3])
- 조건부 실행

**예시**: `/init`, `/setup`, `/configure`

---

## 4. Pipeline

순차적 단계 실행. 배포/릴리스에 적합합니다.

**적합한 경우**:
- 순서 의존성 있는 작업
- 각 단계별 검증 필요
- 중간 실패 시 재개 가능

**핵심 요소**:
- Stage 1, 2, 3... 순차 진행
- 각 Stage 완료 조건 명시
- `--stage=N` 재개 옵션

**예시**: `/deploy`, `/release`, `/migration`

---

## 유형 선택 가이드

```
작업이 단일인가?
├─ Yes → Simple
└─ No
    ├─ 병렬 처리 가능? → Batch Processor
    ├─ 사용자 입력 필요? → Interactive
    └─ 순서 의존성? → Pipeline
```

## 조합 패턴

유형은 조합 가능:

- **Interactive + Pipeline**: 설정 질문 후 단계별 실행
- **Pipeline + Batch**: 각 Stage에서 배치 처리
- **Interactive + Batch**: 옵션 선택 후 병렬 처리
