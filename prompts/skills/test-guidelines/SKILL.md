---
name: test-guidelines
description: 테스트 작성 원칙과 모범 사례를 제공합니다. 테스트 코드 작성, 테스트 검증, 테스트 리뷰가 필요할 때 사용하세요. (project, gitignored)
---

# Test Guidelines

테스트 작성의 핵심 원칙과 워크플로우를 제공하는 스킬입니다.

**핵심 철학**:
- 실제 구현 우선, Mock은 최후의 수단
- 작은 단위부터 시작 (Bottom-up)
- 반복되는 패턴은 fixture/factory로 재사용
- Mock 사용 시 반드시 사용자 승인 필요
- **테스트 실패 = 실제 문제. 절대 우회하지 말 것**

## Instructions

### Workflow 1: 새 테스트 파일 생성

1. **테스트 대상 코드 분석**
   - Glob으로 관련 파일 찾기 (`*.test.*`, `*_test.*` 등)
   - **구현 코드 먼저 Read** (테스트 작성 전 필수)
   - Grep으로 기존 테스트 패턴 확인
   - **가장 작은 단위부터** (순수 함수, 단일 메서드)
   - 함수/메서드의 입력, 출력, 부수 효과 파악
   - 의존성 확인 (DB, 외부 API, 파일 시스템 등)

2. **테스트 전략 수립**
   - AAA 패턴 (Arrange-Act-Assert) 적용
   - 테스트 케이스 목록: Happy path, Edge cases, Error cases
   - 사용자에게 전략 설명 및 확인

3. **⚠️ Mock 필요성 판단 및 사용자 확인**
   - Mock이 필요한 경우 반드시 사용자에게 물어보기
   - 대안 제시 (실제 구현, Test Double, Fake)
   - **사용자 승인 없이 절대 Mock 사용 금지**

4. **Fixture/Factory 패턴 고려**
   - 반복되는 테스트 데이터 패턴 확인
   - 동일 데이터 3회 이상 반복 시 Factory 제안
   - 언어별 가이드에서 구현 방법 참조

5. **테스트 코드 작성**
   - Write tool로 테스트 파일 생성
   - **점진적 작성**: 테스트 1개 작성 → 실행 → 다음 테스트
   - 명확한 테스트명 사용 (무엇을_언제_어떻게)
   - 각 테스트는 독립적으로 실행 가능하게

6. **테스트 실행 및 검증**
   - Bash tool로 테스트 실행
   - **실패 시**: 테스트 우회 금지, 실제 구현 문제 해결 (원칙 5 참조)
   - 사용자에게 결과 보고

### Workflow 2: 기존 테스트 리뷰/수정

1. **테스트 파일 읽기**
   - Glob으로 테스트 파일 목록 확인
   - 큰 파일(>500줄)은 Grep으로 구조 먼저 파악
   - Read tool로 테스트 코드 분석 (필요 시 offset/limit)
   - 구현 코드도 함께 Read (테스트만 보지 말 것)
   - 중복 패턴 식별

2. **품질 체크리스트 적용**
   - 테스트 독립성, 명확성, AAA 패턴 확인
   - Mock 과다 사용 검토
   - 재사용성 확인

3. **테스트 우회 패턴 찾기 (⚠️ 중요)**
   - 테스트 우회 패턴 검색 (원칙 5 참조)
   - **발견 시 즉시 사용자에게 보고**

4. **개선 제안 및 사용자 확인**
   - 발견된 문제점 나열
   - 구체적인 개선 방법 제시
   - 사용자 확인 후 수정

### Workflow 3: 실패 테스트 디버깅

1. **실패 정보 수집**
   - Bash로 verbose 실행, 정확한 에러 메시지 확인
   - 어떤 assertion이 실패했는지 파악

2. **코드 분석**
   - 구현과 테스트 모두 Read (아직 안 읽었다면)
   - 실패 원인 판단: 구현 버그 vs 테스트 문제

3. **문제 해결**
   - 구현 버그 → 구현 코드 수정
   - 스펙 불명확 → 사용자에게 스펙 확인
   - 테스트 과다 엄격 → 사용자 승인 후 조정
   - **절대 테스트 우회 금지** (원칙 5 참조)

## 테스트 작성 원칙

### 1. 작은 단위부터 시작 (Bottom-up)

**올바른 순서**: 순수 함수 → 단일 클래스 → 모듈 통합 → E2E

**작은 단위의 장점**:
- 빠른 피드백 (밀리초 단위)
- 명확한 실패 원인
- 쉬운 디버깅

### 2. AAA 패턴 (Arrange-Act-Assert)

- **Arrange**: 데이터/객체 준비, Fixture/Factory 활용
- **Act**: 테스트 대상 **단 한 번** 호출
- **Assert**: 예상 결과 비교, 부수 효과 확인

### 3. 테스트 독립성

- 각 테스트는 독립적으로 실행
- 테스트 간 실행 순서 무관
- Fixture는 각 테스트마다 fresh하게

### 4. 명확한 테스트명

**패턴**: `메서드명_상황_예상결과`

**좋은 예**:
- `calculateTotal_withValidInput_returnsCorrectSum`
- `fetchUser_whenUserNotFound_throwsNotFoundError`

### 5. 테스트 실패 대응 원칙 (⚠️ 중요)

```
❌ 절대 금지:
- 테스트 주석 처리
- skip, xfail, disabled 사용
- 조건부 assert (if 문 안에 assert)
- 테스트 조건을 스펙과 무관하게 수정
- 항상 통과하는 의미 없는 assert
- Mock으로 무작정 우회

✅ 올바른 대응:
1. 실패 원인 정확히 파악
2. 구현 코드 수정 (스펙 준수)
3. 스펙이 잘못됐다면 사용자와 논의
4. 테스트는 실패 상태로 유지하고 이슈 추적
```

**테스트는 거짓말하지 않는다**:
- 테스트 실패 = 구현 문제 or 스펙 오해
- 테스트를 수정해서 통과시키는 것은 자기기만
- 실패하는 테스트가 통과하는 거짓 테스트보다 낫다

## Fixture & Factory 패턴

### Fixture
- 테스트에 필요한 고정된 상태/데이터
- Setup/Teardown으로 관리
- 각 테스트마다 fresh한 상태

### Factory 패턴
- 테스트 데이터 생성 재사용 함수/클래스
- 기본값 + 필요 시 override
- 동일 데이터 3회 이상 반복 시 사용

### 재사용성 판단

```
✅ Fixture/Factory 만들기:
- 동일 데이터 3회 이상 반복
- 복잡한 객체 생성 (5개 이상 속성)
- Setup/Teardown 공통

❌ 불필요:
- 1-2개 테스트에서만 사용
- 매우 간단한 데이터
```

## Mock/Stub 사용 철학

### ⚠️ Mock은 최후의 수단

**우선순위**:
1. 실제 구현 사용
2. 가벼운 Test Double (In-memory DB, Fake, Stub)
3. ⚠️ 사용자 허락 후 Mock 사용

### Mock 사용 정당화

```
✅ Mock 필요:
- 외부 API (결제, 이메일)
- 파일 시스템 대량 I/O
- 현재 시간 의존성
- 네트워크 요청
- 느린 연산 (>1초)

❌ Mock 불필요:
- 단순 함수 (순수 함수)
- 내부 모듈/클래스
- 쉽게 재현 가능한 로직
- 빠른 계산 (< 100ms)
- 프로젝트 내부 의존성
```

### Test Double 유형

1. Dummy: 전달만 됨
2. Stub: 미리 정의된 응답
3. Spy: 호출 기록 + 실제 동작
4. **Mock**: 행위 검증 (사용자 승인 필요)
5. Fake: 간단한 구현

## 언어별 가이드

실제 코드 구현 방법은 언어별 가이드 참조:

- **IF Python**: `./languages/python-test-guide.md`
  (pytest, fixture, unittest.mock)

- **IF TypeScript/JavaScript**: `./languages/typescript-test-guide.md`
  (Jest, Vitest, Factory 함수, jest.fn)

- **IF Ruby**: `./languages/ruby-test-guide.md`
  (RSpec, Minitest, Factory Bot)

- **IF Go**: `./languages/go-test-guide.md`
  (testing, Table-driven tests)

- **IF Java**: `./languages/java-test-guide.md`
  (JUnit 5, Mockito)

각 가이드 포함: 프레임워크 사용법, 코드 예제, Fixture/Factory 구현, Mock 라이브러리

## 품질 체크리스트

### 테스트 구조
- [ ] 작은 단위부터 테스트 (순수 함수 → 클래스 → 통합)
- [ ] AAA 패턴 명확
- [ ] 독립적 실행 가능
- [ ] 명확한 테스트명
- [ ] 단일 관심사

### 테스트 진정성 (⚠️ 중요)
- [ ] 주석 처리된 테스트 없음
- [ ] skip/disabled 테스트 없음
- [ ] 조건부 assert 없음 (if 문 안에 assert)
- [ ] 실제 스펙 검증
- [ ] 의미 있는 assert
- [ ] Mock 우회 없음

### 재사용성
- [ ] Fixture/Factory 적절히 사용
- [ ] 공통 Setup/Teardown 정리

### Mock 사용
- [ ] 사용자 승인 받음
- [ ] 정말 필요한 부분에만 사용
- [ ] 최소한으로 사용

### 테스트 품질
- [ ] 빠른 실행 (< 1초)
- [ ] 명확한 실패 원인
- [ ] Edge case/error case 검증
- [ ] Flaky 아님 (항상 같은 결과)

## Technical Details

언어별 구체적 구현은 `./languages/` 디렉토리 참조.
