---
# Placeholders:
#   {{TASK_NAME}}: kebab-case 작업명 (예: feature-rate-limiter)
#   {{PHASE}}: SPECIFY | PLAN | IMPLEMENT | COMPLETE
#   {{DATE}}: YYYY-MM-DD 형식
#   {{TIMESTAMP}}: YYYY-MM-DD HH:MM 형식
task: {{TASK_NAME}}
phase: {{PHASE}}
created: {{DATE}}
updated: {{DATE}}
retry_count: 0
last_error_hash: null
---

# Auto Dev State

## 현재 상태

- **Phase**: {{PHASE}}
- **진행률**: 0%
- **현재 task**: (없음)
- **마지막 작업**: 시작 전

## 완료 조건

SPECIFY phase에서 사용자와 확정한 테스트 목록:

- [ ] 조건 1
- [ ] 조건 2

## 진행 기록

### {{TIMESTAMP}}: Phase 시작

- **작업**: SPECIFY 시작
- **결과**: 진행 중

## 블로커 (있는 경우)

- **에러**: 없음
- **시도 횟수**: 0/3
- **에러 해시**: null
- **다음 시도**: (해당 없음)

## 요구사항 확정 내역

SPECIFY phase에서 사용자에게 질문하여 확정한 내용:

| 질문 | 답변 |
|------|------|
| (예: 저장 방식?) | (예: localStorage) |
