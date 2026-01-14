---
description: 테스트 핵심 원칙
paths:
  - "**/*.test.{ts,tsx,js,jsx}"
  - "**/*.spec.{ts,tsx,js,jsx}"
  - "**/*_test.py"
  - "**/test_*.py"
  - "**/*_test.go"
  - "**/*_test.rs"
---

모든 동작은 테스트 코드로 검증합니다.

## 핵심 원칙

- **행위(behavior) 테스트**: 구현 세부사항 ❌, 입출력 결과만 ✅
- **공개 API만 테스트**: private 직접 테스트 금지
- **리팩토링 내성**: 내부 변경해도 테스트 안 깨져야 함
- **Mock 최소화**: 아키텍처 경계(외부 API, I/O)에서만
- **우회 금지**: skip, xfail, 주석처리 금지 → 구현 수정으로 해결

## 상세 가이드

테스트 작성/리뷰 시 `/test-guidelines` 참조
