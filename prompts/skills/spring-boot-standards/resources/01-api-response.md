# API Response Pattern

## ApiResult 래퍼

모든 REST API 응답은 `ApiResult<T>` 래퍼를 사용하여 일관된 구조를 유지합니다.

### ApiResult 클래스

```java
@Getter
@Builder
public class ApiResult<T> {
    private final boolean success;
    private final String message;
    private final T data;
    private final List<FieldError> errors;

    // 성공 응답
    public static <T> ApiResult<T> success(T data) {
        return ApiResult.<T>builder()
            .success(true)
            .data(data)
            .build();
    }

    public static <T> ApiResult<T> success(T data, String message) {
        return ApiResult.<T>builder()
            .success(true)
            .data(data)
            .message(message)
            .build();
    }

    // 에러 응답
    public static <T> ApiResult<T> error(String message) {
        return ApiResult.<T>builder()
            .success(false)
            .message(message)
            .build();
    }

    public static <T> ApiResult<T> error(String message, List<FieldError> errors) {
        return ApiResult.<T>builder()
            .success(false)
            .message(message)
            .errors(errors)
            .build();
    }

    @Getter
    @Builder
    public static class FieldError {
        private final String field;
        private final String message;
    }
}
```

---

## 응답 구조

### 성공 응답

```json
{
  "success": true,
  "data": {
    "id": 1,
    "name": "홍길동",
    "email": "hong@example.com"
  },
  "message": "사용자가 생성되었습니다."
}
```

### 에러 응답

```json
{
  "success": false,
  "message": "입력값이 올바르지 않습니다.",
  "errors": [
    { "field": "email", "message": "이메일 형식이 올바르지 않습니다." },
    { "field": "name", "message": "이름은 필수입니다." }
  ]
}
```

### 목록 응답 (페이징)

```json
{
  "success": true,
  "data": {
    "content": [...],
    "totalElements": 100,
    "totalPages": 5,
    "currentPage": 0,
    "size": 20
  }
}
```

---

## Controller 적용

### 올바른 패턴 (✅)

```java
@Tag(name = "사용자 관리")
@RestController
@RequestMapping("/api/v1/users")
@RequiredArgsConstructor
public class UserController {

    private final UserService userService;
    private final MessageSourceService messageSource;

    @Operation(summary = "사용자 생성")
    @PostMapping
    public ResponseEntity<ApiResult<UserResponse>> createUser(
            @Valid @RequestBody CreateUserRequest request) {

        UserResponse response = userService.create(request);
        String message = messageSource.getMessage("user.created");

        return ResponseEntity
            .status(HttpStatus.CREATED)
            .body(ApiResult.success(response, message));
    }

    @Operation(summary = "사용자 목록 조회")
    @GetMapping
    public ResponseEntity<ApiResult<Page<UserResponse>>> getUsers(
            @RequestParam(defaultValue = "0") int page,
            @RequestParam(defaultValue = "20") int size) {

        Page<UserResponse> response = userService.getAll(page, size);

        return ResponseEntity.ok(ApiResult.success(response));
    }

    @Operation(summary = "사용자 단건 조회")
    @GetMapping("/{id}")
    public ResponseEntity<ApiResult<UserResponse>> getUser(
            @PathVariable Long id) {

        UserResponse response = userService.getById(id);

        return ResponseEntity.ok(ApiResult.success(response));
    }

    @Operation(summary = "사용자 수정")
    @PutMapping("/{id}")
    public ResponseEntity<ApiResult<UserResponse>> updateUser(
            @PathVariable Long id,
            @Valid @RequestBody UpdateUserRequest request) {

        UserResponse response = userService.update(id, request);
        String message = messageSource.getMessage("user.updated");

        return ResponseEntity.ok(ApiResult.success(response, message));
    }

    @Operation(summary = "사용자 삭제")
    @DeleteMapping("/{id}")
    public ResponseEntity<ApiResult<Void>> deleteUser(
            @PathVariable Long id) {

        userService.delete(id);
        String message = messageSource.getMessage("user.deleted");

        return ResponseEntity.ok(ApiResult.success(null, message));
    }
}
```

### 잘못된 패턴 (❌)

```java
// ❌ ApiResult 없이 직접 반환
@GetMapping
public ResponseEntity<List<UserResponse>> getUsers() {
    return ResponseEntity.ok(userService.getAll());
}

// ❌ Map으로 임의 구조 반환
@PostMapping
public ResponseEntity<Map<String, Object>> createUser(...) {
    return ResponseEntity.ok(Map.of(
        "status", "success",
        "user", response
    ));
}

// ❌ 메시지 하드코딩
@PostMapping
public ResponseEntity<ApiResult<UserResponse>> createUser(...) {
    return ResponseEntity.ok(ApiResult.success(response, "생성됨"));
}
```

---

## HTTP 상태 코드 매핑

| 상황 | HTTP Status | ApiResult |
|-----|-------------|-----------|
| 생성 성공 | 201 Created | success: true |
| 조회 성공 | 200 OK | success: true |
| 수정 성공 | 200 OK | success: true |
| 삭제 성공 | 200 OK | success: true |
| 유효성 검증 실패 | 400 Bad Request | success: false |
| 인증 실패 | 401 Unauthorized | success: false |
| 권한 없음 | 403 Forbidden | success: false |
| 리소스 없음 | 404 Not Found | success: false |
| 비즈니스 예외 | 200 OK* | success: false |
| 시스템 오류 | 500 Internal Server Error | success: false |

> *비즈니스 예외는 정상적인 비즈니스 플로우이므로 200 OK + success: false 반환 가능

---

## 체크리스트

새 Controller 생성 시:

- [ ] 모든 엔드포인트가 `ResponseEntity<ApiResult<T>>` 반환
- [ ] MessageSourceService 주입하여 메시지 처리 → `03-i18n.md` 참조
- [ ] 생성(POST)은 201 Created 반환
- [ ] Swagger 어노테이션 추가 (`@Tag`, `@Operation`)
- [ ] 로깅 표준 적용 → `02-logging.md` 참조
