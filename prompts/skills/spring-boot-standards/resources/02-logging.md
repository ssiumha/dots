# Logging Standards

## HTTP 메서드별 로그 레벨

| HTTP Method | Log Level | 이유 |
|-------------|-----------|------|
| GET | DEBUG | 조회는 빈번함, 노이즈 감소 |
| POST | INFO | 생성 작업은 추적 필요 |
| PUT/PATCH | INFO | 수정 작업은 추적 필요 |
| DELETE | WARN | 삭제는 감사 대상, 주의 필요 |

---

## 로그 포맷

### 표준 패턴

```
패턴: "HTTP_METHOD /full/path - 작업설명: param1={}, param2={}"
```

### 예시

```java
@Slf4j
@RestController
@RequestMapping("/api/v1/users")
public class UserController {

    // GET: DEBUG 레벨
    @GetMapping
    public ResponseEntity<ApiResult<Page<UserResponse>>> getUsers(
            @RequestParam(required = false) String status,
            @RequestParam(defaultValue = "0") int page) {

        log.debug("GET /api/v1/users - 목록 조회: status={}, page={}", status, page);
        // ...
    }

    // GET 단건: DEBUG 레벨
    @GetMapping("/{id}")
    public ResponseEntity<ApiResult<UserResponse>> getUser(@PathVariable Long id) {
        log.debug("GET /api/v1/users/{} - 단건 조회", id);
        // ...
    }

    // POST: INFO 레벨
    @PostMapping
    public ResponseEntity<ApiResult<UserResponse>> createUser(
            @Valid @RequestBody CreateUserRequest request) {

        log.info("POST /api/v1/users - 생성 요청: email={}", request.getEmail());
        // ...
        log.info("POST /api/v1/users - 생성 완료: id={}", response.getId());
    }

    // PUT: INFO 레벨
    @PutMapping("/{id}")
    public ResponseEntity<ApiResult<UserResponse>> updateUser(
            @PathVariable Long id,
            @Valid @RequestBody UpdateUserRequest request) {

        log.info("PUT /api/v1/users/{} - 수정 요청", id);
        // ...
        log.info("PUT /api/v1/users/{} - 수정 완료", id);
    }

    // DELETE: WARN 레벨
    @DeleteMapping("/{id}")
    public ResponseEntity<ApiResult<Void>> deleteUser(@PathVariable Long id) {
        log.warn("DELETE /api/v1/users/{} - 삭제 요청", id);
        // ...
        log.warn("DELETE /api/v1/users/{} - 삭제 완료", id);
    }
}
```

---

## 예외 로깅

### 레벨별 사용

```java
// 비즈니스 예외: WARN
log.warn("비즈니스 예외 발생: userId={}, reason={}", userId, ex.getMessage());

// 시스템 오류: ERROR (스택트레이스 포함)
log.error("시스템 오류 발생: {}", ex.getMessage(), ex);

// 외부 API 실패: WARN 또는 ERROR
log.warn("외부 API 호출 실패: url={}, status={}", url, status);
```

### GlobalExceptionHandler 로깅

예외 처리 시 로깅 레벨 기준:

| 예외 유형 | 로그 레벨 | 스택트레이스 |
|----------|----------|-------------|
| 비즈니스 예외 | WARN | 미포함 |
| 유효성 검증 실패 | WARN | 미포함 |
| 시스템 오류 | ERROR | 포함 |

```java
// 비즈니스 예외: WARN (스택트레이스 없음)
log.warn("비즈니스 예외: code={}, message={}", ex.getErrorCode(), ex.getMessage());

// 유효성 검증: WARN
log.warn("유효성 검증 실패: fields={}", fieldNames);

// 시스템 오류: ERROR (스택트레이스 포함)
log.error("시스템 오류 발생: {}", ex.getMessage(), ex);
```

> 전체 GlobalExceptionHandler 구현은 `04-exception.md` 참조

---

## 민감 정보 마스킹

```java
// ❌ 민감 정보 노출
log.info("로그인 요청: email={}, password={}", email, password);

// ✅ 민감 정보 마스킹
log.info("로그인 요청: email={}", email);
// password는 로깅하지 않음

// ✅ 부분 마스킹
log.info("결제 요청: cardNumber=****{}", cardNumber.substring(12));
```

### 마스킹 대상

- 비밀번호, PIN
- 카드번호, 계좌번호
- 주민등록번호
- API 키, 토큰
- 개인 연락처 (선택적)

---

## Logback 설정

### logback-spring.xml

```xml
<?xml version="1.0" encoding="UTF-8"?>
<configuration>
    <springProperty scope="context" name="APP_NAME" source="spring.application.name"/>

    <!-- 콘솔 출력 패턴 -->
    <appender name="CONSOLE" class="ch.qos.logback.core.ConsoleAppender">
        <encoder>
            <pattern>[%d{yyyy-MM-dd HH:mm:ss.SSS}] [%level] [%thread] [trace_id=%X{trace_id}] [%logger{36}] - %msg%n</pattern>
        </encoder>
    </appender>

    <!-- 파일 출력 (일별 롤링) -->
    <appender name="FILE" class="ch.qos.logback.core.rolling.RollingFileAppender">
        <file>logs/${APP_NAME}.log</file>
        <rollingPolicy class="ch.qos.logback.core.rolling.TimeBasedRollingPolicy">
            <fileNamePattern>logs/${APP_NAME}.%d{yyyy-MM-dd}.log</fileNamePattern>
            <maxHistory>30</maxHistory>
        </rollingPolicy>
        <encoder>
            <pattern>[%d{yyyy-MM-dd HH:mm:ss.SSS}] [%level] [%thread] [trace_id=%X{trace_id}] [%logger{36}] - %msg%n</pattern>
        </encoder>
    </appender>

    <!-- 프로파일별 설정 -->
    <springProfile name="local">
        <root level="DEBUG">
            <appender-ref ref="CONSOLE"/>
        </root>
    </springProfile>

    <springProfile name="dev,staging">
        <root level="INFO">
            <appender-ref ref="CONSOLE"/>
            <appender-ref ref="FILE"/>
        </root>
    </springProfile>

    <springProfile name="prod">
        <root level="WARN">
            <appender-ref ref="FILE"/>
        </root>
        <logger name="com.company.project" level="INFO"/>
    </springProfile>
</configuration>
```

---

## 체크리스트

- [ ] GET 요청은 DEBUG 레벨
- [ ] POST/PUT/PATCH 요청은 INFO 레벨
- [ ] DELETE 요청은 WARN 레벨
- [ ] 민감 정보 마스킹 처리
- [ ] 예외 로깅 시 적절한 레벨 사용
- [ ] trace_id MDC 설정 (분산 추적)
