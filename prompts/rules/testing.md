---
description: 테스트 규칙
paths: "**/*.{ts,tsx,js,jsx,py,go,rs}"
---

모든 동작은 테스트 코드로 검증합니다.

- 프로젝트 표준 테스트 프레임워크 사용 (pytest, jest, go test 등)
- E2E 테스트 외에는 개발 서버를 임의로 띄우지 않음
- 테스트 실행 후 결과 확인 필수
