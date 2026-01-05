# [엔터프라이즈 프로젝트명]

[프로젝트 설명: 대규모 엔터프라이즈 애플리케이션]

**스택**: [Framework] + [Database] + [Cache] + [Message Queue]
**Base Package**: com.company.project

## Quick Commands

```bash
# === Build & Run ===
./gradlew build                   # 전체 빌드
./gradlew bootRun                 # 개발 서버
# → http://localhost:8080

# === Testing ===
./gradlew test                    # 유닛 테스트
./gradlew integrationTest         # 통합 테스트 (Testcontainers)
./gradlew test --tests "*.UserServiceTest"  # 특정 테스트

# === Code Quality ===
./gradlew checkstyleMain          # Checkstyle
./gradlew spotbugsMain            # SpotBugs
./gradlew pmdMain                 # PMD
./gradlew jacocoTestReport        # 커버리지 리포트

# === CI 전체 ===
just ci                           # lint + test + build
```

## Services

### Development

- API: http://localhost:8080
- Health: http://localhost:8080/actuator/health
- Swagger: http://localhost:8080/swagger-ui.html
- PostgreSQL: localhost:5432
- Redis: localhost:6379
- Kafka: localhost:9092

### Production

- API: https://api.example.com
- Health: https://api.example.com/actuator/health

## Environment Variables

### Required

```bash
# .env
SPRING_PROFILES_ACTIVE=local

# Database
DATABASE_URL=jdbc:postgresql://localhost:5432/app_dev
DATABASE_USERNAME=postgres
DATABASE_PASSWORD=postgres

# Redis
REDIS_HOST=localhost
REDIS_PORT=6379

# Kafka
KAFKA_BOOTSTRAP_SERVERS=localhost:9092

# Security
JWT_SECRET=your-256-bit-secret
```

### Optional

```bash
LOG_LEVEL=DEBUG
SWAGGER_ENABLED=true
```

## Architecture

### Package Structure

```
src/main/java/com/company/project/
├── domain/                      # Domain Layer (핵심 비즈니스)
│   ├── model/                   # Aggregate Root, Entity, Value Object
│   ├── repository/              # Repository 인터페이스 (포트)
│   ├── service/                 # Domain Service
│   └── event/                   # Domain Event
├── application/                 # Application Layer (유스케이스)
│   ├── dto/                     # Request/Response DTO
│   ├── service/                 # Application Service (유스케이스 조합)
│   └── mapper/                  # Entity ↔ DTO 변환
├── infrastructure/              # Infrastructure Layer (어댑터)
│   ├── persistence/             # Repository 구현체, JPA Entity
│   ├── external/                # 외부 API 클라이언트
│   └── messaging/               # Kafka Producer/Consumer
└── presentation/                # Presentation Layer (API)
    ├── controller/              # REST Controller
    ├── advice/                  # Exception Handler
    └── config/                  # Web Config, Security Config
```

### Layer Dependencies

```
Presentation → Application → Domain ← Infrastructure
                   ↓
            Infrastructure (구현)

- Domain: 순수 비즈니스 로직, 외부 의존 없음
- Application: 유스케이스 조합, 트랜잭션 경계
- Infrastructure: DB, 외부 API 구현
- Presentation: HTTP 처리
```

## REST API Standards

### Response Pattern

모든 API는 `ApiResult<T>` 래퍼 사용:

```java
// ✅ 올바른 패턴: ApiResult 래퍼 사용
@PostMapping
public ResponseEntity<ApiResult<UserResponse>> createUser(
        @Valid @RequestBody CreateUserRequest request) {
    UserResponse response = userService.create(request);
    String message = messageSource.getMessage("user.created");
    return ResponseEntity
        .status(HttpStatus.CREATED)
        .body(ApiResult.success(response, message));
}

// ❌ 잘못된 패턴: ApiResult 없이 직접 반환
@GetMapping
public ResponseEntity<List<UserResponse>> getUsers() {
    return ResponseEntity.ok(userService.getAll());  // 이렇게 하지 마세요
}
```

### ApiResult Structure

```json
{
  "success": true,
  "data": { ... },
  "message": "성공적으로 생성되었습니다."
}

{
  "success": false,
  "message": "입력값이 올바르지 않습니다.",
  "errors": [
    { "field": "email", "message": "이메일 형식이 올바르지 않습니다." }
  ]
}
```

## Logging Standards

### HTTP Method별 로그 레벨

```java
// ✅ GET 요청: debug 레벨 (조회는 빈번함)
@GetMapping
public ResponseEntity<ApiResult<List<UserResponse>>> getUsers(...) {
    log.debug("GET /api/v1/users - 목록 조회: filter={}", filter);
}

// ✅ POST/PUT/DELETE: info 레벨 (변경 추적 필요)
@PostMapping
public ResponseEntity<ApiResult<UserResponse>> createUser(...) {
    log.info("POST /api/v1/users - 생성 요청: email={}", request.getEmail());
}

// ✅ 예외 발생: warn 레벨 (비즈니스 예외)
log.warn("비즈니스 예외 발생: userId={}, reason={}", userId, reason);

// ✅ 시스템 오류: error 레벨
log.error("시스템 오류 발생: {}", e.getMessage(), e);
```

### Log Format

```
패턴: "HTTP_METHOD /full/path - 작업설명: param1={}, param2={}"
예시: "POST /api/v1/users - 생성 요청: email=test@example.com"
```

## Internationalization (i18n)

### MessageSource 사용

```java
// ✅ 올바른 패턴: MessageSource 사용
@RequiredArgsConstructor
public class UserService {
    private final MessageSourceService messageSource;

    public void createUser(...) {
        String message = messageSource.getMessage("user.created");
        return ApiResult.success(response, message);
    }
}

// ❌ 잘못된 패턴: 하드코딩
return ApiResult.success(response, "사용자가 생성되었습니다.");  // 이렇게 하지 마세요
```

### Message File Structure

```
resources/
├── messages.properties           # 기본 (한국어)
├── messages_ko.properties        # 한국어 명시
└── messages_en.properties        # 영어
```

### Message Key Naming

```properties
# 패턴: {도메인}.{기능}.{상태}
user.created=사용자가 생성되었습니다.
user.updated=사용자 정보가 수정되었습니다.
user.deleted=사용자가 삭제되었습니다.
user.notfound=사용자를 찾을 수 없습니다.

order.created=주문이 접수되었습니다.
order.cancelled=주문이 취소되었습니다.
```

## Swagger Documentation

### Required Annotations

```java
@Tag(name = "사용자 관리", description = "사용자 CRUD API")
@RestController
@RequestMapping("/api/v1/users")
public class UserController {

    @Operation(summary = "사용자 생성", description = "새로운 사용자를 생성합니다.")
    @ApiResponses({
        @ApiResponse(responseCode = "201", description = "생성 성공"),
        @ApiResponse(responseCode = "400", description = "잘못된 요청"),
        @ApiResponse(responseCode = "409", description = "중복된 이메일")
    })
    @PostMapping
    public ResponseEntity<ApiResult<UserResponse>> createUser(...) { }
}
```

## Checklists

### New Controller Checklist

- [ ] **ApiResult 패턴 적용**: 모든 엔드포인트가 `ResponseEntity<ApiResult<T>>` 반환
- [ ] **MessageSourceService 주입**: 다국어 메시지 처리를 위해 필수
- [ ] **Swagger 문서화**: `@Tag`, `@Operation`, `@ApiResponses` 어노테이션 추가
- [ ] **로깅 표준 준수**: GET은 `debug`, POST/PUT/DELETE는 `info` 레벨
- [ ] **메시지 파일 업데이트**: messages_ko.properties와 messages_en.properties에 메시지 키 추가
- [ ] **입력 검증**: `@Valid` 어노테이션과 DTO 검증 규칙 추가
- [ ] **권한 체크**: 필요시 `@PreAuthorize` 어노테이션 추가

### New Service Checklist

- [ ] **트랜잭션 경계 설정**: 읽기 전용은 `@Transactional(readOnly = true)`
- [ ] **예외 처리**: 비즈니스 예외는 커스텀 예외 클래스 사용
- [ ] **로깅**: 주요 비즈니스 로직에 로그 추가
- [ ] **테스트 작성**: 단위 테스트 필수, 통합 테스트 권장

### PR Checklist

- [ ] **테스트 통과**: 모든 기존/신규 테스트 통과
- [ ] **코드 품질**: Checkstyle, SpotBugs, PMD 통과
- [ ] **문서화**: Swagger 어노테이션, 메시지 키 추가
- [ ] **리뷰 요청**: 최소 1명 리뷰어 지정

## Testing Strategy

### Test Directory Structure (DDD)

```
src/test/java/com/company/project/
├── domain/                      # Domain Layer 테스트
│   ├── model/                   # Entity, VO 테스트 (순수 단위 테스트)
│   └── service/                 # Domain Service 테스트
├── application/                 # Application Layer 테스트
│   └── service/                 # 유스케이스 테스트 (Mock 사용)
├── infrastructure/              # Infrastructure Layer 테스트
│   ├── persistence/             # Repository 테스트 (Testcontainers)
│   └── external/                # 외부 API 테스트 (WireMock)
└── presentation/                # Presentation Layer 테스트
    └── controller/              # Controller 테스트 (MockMvc)
```

### Test Pyramid

- **Unit Test (70%)**: Domain 계층, 순수 비즈니스 로직
- **Integration Test (20%)**: Repository, 외부 API 연동
- **E2E Test (10%)**: 전체 플로우 검증

### Testcontainers Example

```java
@SpringBootTest
@Testcontainers
class UserRepositoryTest {
    @Container
    static PostgreSQLContainer<?> postgres =
        new PostgreSQLContainer<>("postgres:15-alpine");

    @Container
    static GenericContainer<?> redis =
        new GenericContainer<>("redis:7-alpine")
            .withExposedPorts(6379);
}
```

## Troubleshooting

### Swagger UI 접근 불가

**증상**: http://localhost:8080/swagger-ui.html 404

**해결**:
1. `application.yml`에서 `springdoc.swagger-ui.enabled=true` 확인
2. Security 설정에서 `/swagger-ui/**`, `/v3/api-docs/**` 허용 확인
3. 애플리케이션 재시작

### MessageSource 메시지 못 찾음

**증상**: `No message found under code 'user.created'`

**해결**:
1. `messages.properties` 파일 위치 확인: `src/main/resources/`
2. 파일 인코딩 확인: UTF-8
3. 메시지 키 오타 확인

### Testcontainers 연결 실패

**증상**: `Could not create container`

**해결**:
1. Docker 데몬 실행 확인
2. Docker Desktop 리소스 할당 확인 (메모리, CPU)
3. 테스트 시 `@Testcontainers` 어노테이션 확인

## References

- 상세 개발 표준: `docs/development-standards.md`
- 도메인 설계 문서: `docs/domain-design.md`
- API 명세: http://localhost:8080/swagger-ui.html
