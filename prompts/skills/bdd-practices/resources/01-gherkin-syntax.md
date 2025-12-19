# Gherkin Syntax Reference

## 기본 구조

```gherkin
Feature: 기능명
  기능에 대한 설명 (여러 줄 가능)

  Background:
    Given 모든 시나리오 공통 전제조건

  Scenario: 시나리오명
    Given 전제조건
    When 행동
    Then 결과
    And 추가 조건
    But 예외 조건
```

## 핵심 키워드

| 키워드 | 용도 | 예시 |
|--------|------|------|
| `Feature` | 기능 정의 | `Feature: 사용자 인증` |
| `Scenario` | 단일 테스트 케이스 | `Scenario: 유효한 로그인` |
| `Scenario Outline` | 데이터 기반 시나리오 | 아래 참조 |
| `Background` | 공통 전제조건 | `Background:` |
| `Given` | 전제조건 (상태 설정) | `Given 로그인 페이지에 있다` |
| `When` | 행동 (이벤트) | `When 로그인 버튼을 누른다` |
| `Then` | 결과 (검증) | `Then 대시보드가 표시된다` |
| `And` / `But` | 조건 연결 | `And 이름이 표시된다` |

## Scenario Outline (데이터 기반)

```gherkin
Scenario Outline: 입력값 검증
  Given 로그인 페이지에 있다
  When "<email>" 이메일을 입력한다
  And "<password>" 비밀번호를 입력한다
  Then "<result>" 결과가 표시된다

  Examples:
    | email            | password | result       |
    | test@example.com | pass123  | 로그인 성공  |
    | invalid          | pass123  | 이메일 오류  |
    | test@example.com |          | 비밀번호 필요 |
```

## Data Tables

```gherkin
Given 다음 사용자들이 있다:
  | 이름 | 이메일           | 역할  |
  | 철수 | cs@test.com     | admin |
  | 영희 | yh@test.com     | user  |
```

## Doc Strings (멀티라인)

```gherkin
When API에 다음 요청을 보낸다:
  """json
  {
    "email": "test@example.com",
    "password": "secret"
  }
  """
```

## Tags

```gherkin
@smoke @login
Scenario: 빠른 로그인 테스트

@slow @integration
Scenario: 전체 인증 플로우

@wip
Scenario: 작업 중인 시나리오
```

**주요 태그**:
- `@smoke`: 스모크 테스트
- `@wip`: Work in Progress
- `@skip`: 건너뛰기
- `@slow`: 느린 테스트

## 좋은 시나리오 작성 원칙

### DO ✅

```gherkin
# 비즈니스 언어 사용
Scenario: 유효한 자격증명으로 로그인
  Given 등록된 사용자가 있다
  When 올바른 자격증명으로 로그인한다
  Then 대시보드가 표시된다
```

### DON'T ❌

```gherkin
# 구현 세부사항 노출
Scenario: 로그인
  Given users 테이블에 레코드가 있다
  When #login-btn을 클릭한다
  And POST /api/login을 호출한다
  Then 200 OK를 받는다
```

### 더 많은 비교 예시

```gherkin
# ❌ 데이터베이스 세부사항 노출
Given users 테이블에 email="test@example.com" 레코드가 있다
# ✅ 비즈니스 언어
Given "test@example.com" 사용자가 등록되어 있다

# ❌ HTTP 상태 코드 노출
Then 200 OK 응답을 받는다
# ✅ 결과 중심
Then 로그인에 성공한다

# ❌ 여러 행동을 하나의 When에
When 이메일을 입력하고 비밀번호를 입력하고 로그인 버튼을 클릭한다
# ✅ 단일 행동
When 유효한 자격증명으로 로그인한다

# ❌ 기술적 검증
Then localStorage에 "token" 키가 있다
# ✅ 사용자 관점 결과
Then 로그인 상태가 유지된다
```

## 실제 예시

```gherkin
Feature: 장바구니
  사용자가 상품을 장바구니에 추가하고 관리할 수 있다

  Background:
    Given 로그인된 사용자가 있다

  Scenario: 상품 추가
    Given 상품 목록 페이지에 있다
    When "노트북" 상품을 장바구니에 추가한다
    Then 장바구니에 1개 상품이 있다
    And "노트북"이 장바구니에 표시된다

  Scenario: 수량 변경
    Given 장바구니에 "노트북" 1개가 있다
    When 수량을 3으로 변경한다
    Then 장바구니에 "노트북" 3개가 표시된다

  Scenario Outline: 할인 적용
    Given 장바구니에 <price>원 상품이 있다
    When <coupon> 쿠폰을 적용한다
    Then 최종 가격이 <final>원이다

    Examples:
      | price  | coupon | final  |
      | 10000  | 10%    | 9000   |
      | 50000  | 5000원 | 45000  |
```
