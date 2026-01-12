---
name: spring-boot-standards
description: Provides Spring Boot API development standards. Use when implementing response formats, logging, i18n, or exception handling patterns.
---

# Spring Boot Standards

Spring Boot 기반 API 개발의 표준 패턴과 베스트 프랙티스를 제공합니다.

**핵심 철학**:
- 일관된 API 응답 구조 (ApiResult)
- HTTP 메서드별 로깅 레벨 표준
- 다국어 메시지 처리 (MessageSource)
- 체계적인 예외 처리

## Instructions

### 워크플로우: 요청 분석 및 리소스 선택

사용자 요청을 분석하여 필요한 리소스만 선택적으로 로드합니다.

#### 1. 키워드 매칭

**API 응답 패턴** (`resources/01-api-response.md`)
- "ApiResult", "응답 패턴", "response"
- "래퍼", "wrapper", "공통 응답"
- "에러 응답", "성공 응답"

**로깅 표준** (`resources/02-logging.md`)
- "로깅", "logging", "log"
- "로그 레벨", "log level"
- "debug", "info", "warn", "error"

**다국어 메시지** (`resources/03-i18n.md`)
- "i18n", "다국어", "국제화"
- "MessageSource", "메시지"
- "messages.properties"

**예외 처리** (`resources/04-exception.md`)
- "예외", "exception", "에러 처리"
- "ExceptionHandler", "ControllerAdvice"
- "비즈니스 예외", "시스템 예외"

#### 2. 리소스 로딩 전략

**단일 키워드 감지**
- User: "API 응답 패턴 알려줘"
- → Read resources/01-api-response.md

**복합 요청**
- User: "Spring Boot API 표준 전체 설정해줘"
- → Read resources/01-api-response.md
- → Read resources/02-logging.md
- → Read resources/03-i18n.md
- → Read resources/04-exception.md

**불명확한 요청**
- User: "Spring Boot 개발 가이드"
- → 주요 리소스 개요 설명 후 선택지 제시

---

## Quick Reference

### API 응답 패턴

모든 API는 `ApiResult<T>` 래퍼 사용:

```java
// ✅ 올바른 패턴
@PostMapping
public ResponseEntity<ApiResult<UserResponse>> createUser(...) {
    return ResponseEntity.status(HttpStatus.CREATED)
        .body(ApiResult.success(response, message));
}

// ❌ 잘못된 패턴
@GetMapping
public ResponseEntity<List<UserResponse>> getUsers() {
    return ResponseEntity.ok(service.getAll());
}
```

### 로깅 레벨 표준

| HTTP Method | Level | 이유 |
|-------------|-------|------|
| GET | DEBUG | 조회는 빈번, 노이즈 감소 |
| POST | INFO | 생성 추적 필요 |
| PUT/PATCH | INFO | 수정 추적 필요 |
| DELETE | WARN | 삭제는 감사 대상 |

### i18n 메시지

```java
// ✅ MessageSource 사용
messageSource.getMessage("user.created", null, locale);

// ❌ 하드코딩
return "사용자가 생성되었습니다";
```

---

## 예시

### 예시 1: 새 Controller 생성

User: "사용자 관리 API 만들어줘"

1. 키워드 매칭: "API" → API 응답 + 로깅 + i18n
2. Read resources/01-api-response.md
3. Read resources/02-logging.md
4. Read resources/03-i18n.md
5. Controller 생성 (ApiResult 패턴 적용)
6. 로깅 표준 적용
7. messages.properties 업데이트

### 예시 2: 로깅 표준 적용

User: "로깅 어떻게 해야 해?"

1. 키워드 매칭: "로깅" → 로깅 표준
2. Read resources/02-logging.md
3. HTTP 메서드별 로그 레벨 안내
4. 로그 포맷 예시 제공

### 예시 3: 예외 처리 설정

User: "예외 처리 패턴 알려줘"

1. 키워드 매칭: "예외 처리" → 예외 처리
2. Read resources/04-exception.md
3. GlobalExceptionHandler 패턴 제공
4. 비즈니스/시스템 예외 분리 안내

---

## 중요 원칙

1. **일관성**: 모든 API에 동일한 응답 구조
2. **추적성**: 변경 작업은 로그로 추적 가능
3. **다국어**: 사용자 메시지는 하드코딩 금지
4. **예외 분리**: 비즈니스 예외 vs 시스템 예외

## Technical Details

상세한 설정 및 예제는 각 리소스 파일 참조:
- `resources/01-api-response.md`: ApiResult 래퍼 패턴
- `resources/02-logging.md`: HTTP 메서드별 로깅 표준
- `resources/03-i18n.md`: MessageSource 다국어 처리
- `resources/04-exception.md`: GlobalExceptionHandler 패턴
