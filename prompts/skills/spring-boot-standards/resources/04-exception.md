# Exception Handling

## 예외 분류

| 유형 | 설명 | HTTP Status | 로그 레벨 |
|-----|------|-------------|----------|
| 비즈니스 예외 | 정상적인 비즈니스 플로우의 예외 | 200 OK* | WARN |
| 유효성 예외 | 입력값 검증 실패 | 400 Bad Request | WARN |
| 인증 예외 | 인증 실패 | 401 Unauthorized | WARN |
| 권한 예외 | 권한 부족 | 403 Forbidden | WARN |
| 리소스 없음 | 요청 리소스 없음 | 404 Not Found | WARN |
| 시스템 예외 | 예상치 못한 오류 | 500 Internal Server Error | ERROR |

> *비즈니스 예외는 정상 플로우이므로 200 + success: false 가능 (프로젝트 정책에 따름)

---

## 커스텀 예외 클래스

### BusinessException (비즈니스 예외 기본 클래스)

```java
@Getter
public class BusinessException extends RuntimeException {

    private final String errorCode;
    private final HttpStatus httpStatus;

    public BusinessException(String message) {
        super(message);
        this.errorCode = "BUSINESS_ERROR";
        this.httpStatus = HttpStatus.OK;  // 비즈니스 예외는 200
    }

    public BusinessException(String errorCode, String message) {
        super(message);
        this.errorCode = errorCode;
        this.httpStatus = HttpStatus.OK;
    }

    public BusinessException(String errorCode, String message, HttpStatus httpStatus) {
        super(message);
        this.errorCode = errorCode;
        this.httpStatus = httpStatus;
    }
}
```

### 도메인별 예외

```java
// 사용자 관련
public class UserNotFoundException extends BusinessException {
    public UserNotFoundException(Long id) {
        super("USER_NOT_FOUND", "사용자를 찾을 수 없습니다: " + id, HttpStatus.NOT_FOUND);
    }
}

public class DuplicateEmailException extends BusinessException {
    public DuplicateEmailException(String email) {
        super("DUPLICATE_EMAIL", "이미 사용 중인 이메일입니다: " + email);
    }
}

// 인증 관련
public class InvalidCredentialsException extends BusinessException {
    public InvalidCredentialsException() {
        super("INVALID_CREDENTIALS", "이메일 또는 비밀번호가 올바르지 않습니다", HttpStatus.UNAUTHORIZED);
    }
}

public class TokenExpiredException extends BusinessException {
    public TokenExpiredException() {
        super("TOKEN_EXPIRED", "인증이 만료되었습니다", HttpStatus.UNAUTHORIZED);
    }
}

// 권한 관련
public class AccessDeniedException extends BusinessException {
    public AccessDeniedException() {
        super("ACCESS_DENIED", "접근 권한이 없습니다", HttpStatus.FORBIDDEN);
    }
}
```

---

## GlobalExceptionHandler

```java
@RestControllerAdvice
@Slf4j
@RequiredArgsConstructor
public class GlobalExceptionHandler {

    private final MessageSourceService messageSource;

    /**
     * 비즈니스 예외 처리
     */
    @ExceptionHandler(BusinessException.class)
    public ResponseEntity<ApiResult<Void>> handleBusinessException(
            BusinessException ex) {

        log.warn("비즈니스 예외: code={}, message={}",
            ex.getErrorCode(), ex.getMessage());

        return ResponseEntity
            .status(ex.getHttpStatus())
            .body(ApiResult.error(ex.getMessage()));
    }

    /**
     * 유효성 검증 실패 (@Valid)
     */
    @ExceptionHandler(MethodArgumentNotValidException.class)
    public ResponseEntity<ApiResult<Void>> handleValidationException(
            MethodArgumentNotValidException ex) {

        List<ApiResult.FieldError> errors = ex.getBindingResult()
            .getFieldErrors()
            .stream()
            .map(e -> ApiResult.FieldError.builder()
                .field(e.getField())
                .message(e.getDefaultMessage())
                .build())
            .toList();

        log.warn("유효성 검증 실패: fields={}",
            errors.stream().map(ApiResult.FieldError::getField).toList());

        String message = messageSource.getMessage("common.invalid");

        return ResponseEntity
            .status(HttpStatus.BAD_REQUEST)
            .body(ApiResult.error(message, errors));
    }

    /**
     * 타입 변환 실패 (PathVariable, RequestParam)
     */
    @ExceptionHandler(MethodArgumentTypeMismatchException.class)
    public ResponseEntity<ApiResult<Void>> handleTypeMismatch(
            MethodArgumentTypeMismatchException ex) {

        log.warn("타입 변환 실패: param={}, value={}",
            ex.getName(), ex.getValue());

        String message = messageSource.getMessage("common.invalid");

        return ResponseEntity
            .status(HttpStatus.BAD_REQUEST)
            .body(ApiResult.error(message));
    }

    /**
     * HTTP 메서드 불일치
     */
    @ExceptionHandler(HttpRequestMethodNotSupportedException.class)
    public ResponseEntity<ApiResult<Void>> handleMethodNotSupported(
            HttpRequestMethodNotSupportedException ex) {

        log.warn("지원하지 않는 HTTP 메서드: {}", ex.getMethod());

        String message = messageSource.getMessage("common.method.not.allowed");

        return ResponseEntity
            .status(HttpStatus.METHOD_NOT_ALLOWED)
            .body(ApiResult.error(message));
    }

    /**
     * 요청 본문 파싱 실패
     */
    @ExceptionHandler(HttpMessageNotReadableException.class)
    public ResponseEntity<ApiResult<Void>> handleMessageNotReadable(
            HttpMessageNotReadableException ex) {

        log.warn("요청 본문 파싱 실패: {}", ex.getMessage());

        String message = messageSource.getMessage("common.invalid");

        return ResponseEntity
            .status(HttpStatus.BAD_REQUEST)
            .body(ApiResult.error(message));
    }

    /**
     * 시스템 예외 (Fallback)
     */
    @ExceptionHandler(Exception.class)
    public ResponseEntity<ApiResult<Void>> handleException(Exception ex) {
        log.error("시스템 오류 발생: {}", ex.getMessage(), ex);

        String message = messageSource.getMessage("common.error");

        return ResponseEntity
            .status(HttpStatus.INTERNAL_SERVER_ERROR)
            .body(ApiResult.error(message));
    }
}
```

---

## 예외 발생 패턴

### Service에서 예외 발생

```java
@Service
@RequiredArgsConstructor
public class UserService {

    private final UserRepository userRepository;

    public UserResponse getById(Long id) {
        User user = userRepository.findById(id)
            .orElseThrow(() -> new UserNotFoundException(id));

        return UserResponse.from(user);
    }

    public UserResponse create(CreateUserRequest request) {
        // 중복 검사
        if (userRepository.existsByEmail(request.getEmail())) {
            throw new DuplicateEmailException(request.getEmail());
        }

        User user = User.builder()
            .email(request.getEmail())
            .name(request.getName())
            .build();

        return UserResponse.from(userRepository.save(user));
    }
}
```

### 올바른 패턴 vs 잘못된 패턴

```java
// ✅ 올바른 패턴: 커스텀 예외 발생
public User getById(Long id) {
    return userRepository.findById(id)
        .orElseThrow(() -> new UserNotFoundException(id));
}

// ❌ 잘못된 패턴: 일반 예외 발생
public User getById(Long id) {
    return userRepository.findById(id)
        .orElseThrow(() -> new RuntimeException("User not found"));
}

// ❌ 잘못된 패턴: null 반환
public User getById(Long id) {
    return userRepository.findById(id).orElse(null);
}
```

---

## 체크리스트

- [ ] 비즈니스 예외는 BusinessException 상속
- [ ] 도메인별 커스텀 예외 클래스 생성
- [ ] GlobalExceptionHandler 등록
- [ ] 예외 메시지는 MessageSource 사용
- [ ] 시스템 예외는 ERROR 레벨 로깅 (스택트레이스 포함)
- [ ] 비즈니스 예외는 WARN 레벨 로깅
