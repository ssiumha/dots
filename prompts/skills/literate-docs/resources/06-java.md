# Javadoc (Java/Kotlin)

Java와 Kotlin의 표준 문서화 규격입니다.

## 기본 규칙

- `/** ... */` 블록 주석
- 선언 바로 위에 위치
- 첫 문장은 요약 (마침표까지가 요약)
- HTML 태그 사용 가능

## 기본 형식

```java
/**
 * 사용자 정보를 가져온다.
 *
 * @param userId 사용자 ID
 * @return 사용자 객체
 * @throws UserNotFoundException 사용자가 없을 때
 */
public User getUser(String userId) throws UserNotFoundException {
    // ...
}
```

## 주요 태그

### @param

```java
/**
 * 데이터를 처리한다.
 *
 * @param input 입력 데이터 (null 불가)
 * @param options 처리 옵션
 * @param <T> 입력 타입
 * @param <R> 출력 타입
 */
public <T, R> R process(T input, Options options) {
    // ...
}
```

### @return

```java
/**
 * 현재 상태를 반환한다.
 *
 * @return 상태 객체, 초기화 전이면 {@code null}
 */
public Status getStatus() {
    // ...
}
```

### @throws / @exception

```java
/**
 * 파일을 읽는다.
 *
 * @throws IOException 파일 읽기 실패 시
 * @throws IllegalArgumentException path가 null일 때
 * @throws SecurityException 읽기 권한이 없을 때
 */
public byte[] readFile(String path) throws IOException {
    // ...
}
```

### @see

```java
/**
 * 데이터를 저장한다.
 *
 * @see #load(String) 데이터 로드
 * @see DataStore 저장소 인터페이스
 * @see <a href="https://example.com/docs">공식 문서</a>
 */
public void save(String data) {
    // ...
}
```

## 컨텍스트별 문서화

### 패키지 (package-info.java)

```java
/**
 * HTTP 통신을 위한 유틸리티 패키지.
 *
 * <p>이 패키지는 다음 기능을 제공한다:</p>
 * <ul>
 *   <li>HTTP 클라이언트</li>
 *   <li>요청/응답 인터셉터</li>
 *   <li>재시도 로직</li>
 * </ul>
 *
 * <h2>HISTORY</h2>
 * <ul>
 *   <li>2025-01-08: 초기 구현</li>
 *   <li>2025-01-15: HTTP/2 지원</li>
 * </ul>
 *
 * @since 1.0
 */
package com.example.http;
```

### 클래스

```java
/**
 * 사용자를 나타내는 클래스.
 *
 * <p>불변 객체로 설계되었으며, Builder 패턴을 통해 생성한다.</p>
 *
 * <h2>DECISION: Record vs Class</h2>
 * <ul>
 *   <li>이유: Java 16+ 의존성 회피</li>
 *   <li>대안: Lombok @Value (외부 의존성)</li>
 * </ul>
 *
 * @author Alice
 * @version 1.0
 * @since 2025-01-08
 * @see UserBuilder
 */
public final class User {
    // ...
}
```

### 인터페이스

```java
/**
 * 직렬화 가능한 객체를 나타내는 인터페이스.
 *
 * <p>구현체는 스레드 안전해야 한다.</p>
 *
 * <h2>WHY: Serializable 대신 커스텀 인터페이스</h2>
 * <p>Java 직렬화의 보안 이슈 회피, JSON 기반 직렬화 사용.</p>
 *
 * @param <T> 직렬화 대상 타입
 */
public interface Serializer<T> {
    /**
     * 객체를 바이트 배열로 직렬화한다.
     *
     * @param value 직렬화할 객체 (null 불가)
     * @return 직렬화된 바이트 배열
     * @throws SerializationException 직렬화 실패 시
     */
    byte[] serialize(T value) throws SerializationException;
}
```

### Enum

```java
/**
 * 작업 상태를 나타내는 열거형.
 *
 * <p>상태 전이 규칙:</p>
 * <pre>
 * PENDING -> RUNNING -> COMPLETED
 *                   \-> FAILED
 * </pre>
 */
public enum Status {
    /** 대기 중 */
    PENDING,
    /** 실행 중 */
    RUNNING,
    /** 완료됨 */
    COMPLETED,
    /** 실패함 */
    FAILED
}
```

### 필드

```java
public class Config {
    /**
     * 기본 타임아웃 (밀리초).
     *
     * <p>WHY: 30초는 대부분의 API 응답 시간 커버</p>
     */
    public static final int DEFAULT_TIMEOUT = 30000;

    /**
     * 최대 재시도 횟수.
     * @see #setMaxRetries(int)
     */
    private int maxRetries = 3;
}
```

## Kotlin 특화 (KDoc)

### 기본 형식

```kotlin
/**
 * 사용자 정보를 가져온다.
 *
 * @param userId 사용자 ID
 * @return 사용자 객체
 * @throws UserNotFoundException 사용자가 없을 때
 * @sample com.example.Samples.getUserSample
 */
fun getUser(userId: String): User
```

### 프로퍼티

```kotlin
/**
 * 사용자를 나타내는 데이터 클래스.
 *
 * @property id 고유 식별자
 * @property name 표시 이름
 * @property email 이메일 주소 (선택)
 * @constructor 기본 생성자
 */
data class User(
    val id: String,
    val name: String,
    val email: String? = null
)
```

### 코드 블록

```kotlin
/**
 * 목록을 필터링한다.
 *
 * 사용 예시:
 * ```kotlin
 * val filtered = filter(listOf(1, 2, 3)) { it > 1 }
 * // [2, 3]
 * ```
 *
 * @param T 요소 타입
 * @param list 입력 목록
 * @param predicate 필터 조건
 * @return 필터링된 목록
 */
fun <T> filter(list: List<T>, predicate: (T) -> Boolean): List<T>
```

## Codetags 통합

```java
/**
 * 주문을 처리한다.
 *
 * <p>TODO(#123): 배치 처리 지원 추가</p>
 * <p>FIXME: 대용량 주문 시 OOM 발생</p>
 *
 * <h2>WHY: CompletableFuture 대신 Virtual Thread</h2>
 * <ul>
 *   <li>이유: Java 21+ 환경, 간결한 동기 코드</li>
 *   <li>대안: Reactor (학습 곡선), Coroutine (Kotlin 전용)</li>
 * </ul>
 *
 * <h2>DECISION: 낙관적 락 사용</h2>
 * <ul>
 *   <li>Context: 동시성 이슈 해결 필요</li>
 *   <li>Options: 비관적 락 (성능), 낙관적 락 (처리량)</li>
 *   <li>Choice: 낙관적 락</li>
 *   <li>Consequence: 충돌 시 재시도 로직 필요</li>
 * </ul>
 *
 * @param order 주문 정보
 * @return 처리 결과
 */
public Receipt processOrder(Order order) {
    // ...
}
```

## Deprecated

```java
/**
 * 이전 버전의 처리 메서드.
 *
 * @deprecated {@link #processV2(Data)} 사용 권장.
 *             2.0에서 제거 예정.
 */
@Deprecated(since = "1.5", forRemoval = true)
public void process(Data data) {
    // ...
}
```

## HTML 포맷팅

```java
/**
 * 데이터를 변환한다.
 *
 * <p>변환 규칙:</p>
 * <ol>
 *   <li>입력 검증</li>
 *   <li>형식 변환</li>
 *   <li>출력 생성</li>
 * </ol>
 *
 * <p>코드 예시:</p>
 * <pre>{@code
 * Transformer t = new Transformer();
 * Result r = t.transform(input);
 * }</pre>
 *
 * <p><strong>주의:</strong> 스레드 안전하지 않음</p>
 *
 * @param input 입력 데이터
 * @return 변환된 데이터
 */
public Result transform(Input input) {
    // ...
}
```

## 도구 지원

### javadoc 명령

```bash
# 문서 생성
javadoc -d docs -sourcepath src -subpackages com.example

# HTML5 출력
javadoc -html5 -d docs src/**/*.java
```

### Maven

```xml
<plugin>
    <groupId>org.apache.maven.plugins</groupId>
    <artifactId>maven-javadoc-plugin</artifactId>
    <version>3.6.0</version>
</plugin>
```

### Gradle (Kotlin)

```kotlin
tasks.dokkaHtml {
    outputDirectory.set(buildDir.resolve("docs"))
}
```

### IDE

- IntelliJ: `/** + Enter` 자동 생성
- Eclipse: `Alt + Shift + J`

## Best Practices

1. **첫 문장 중요**: 마침표까지가 요약으로 표시됨
2. **null 명시**: 파라미터/반환값의 null 허용 여부
3. **@since 사용**: API 추가 버전 명시
4. **코드 예시**: `{@code ...}` 또는 `<pre>` 블록
5. **링크 활용**: `{@link}`, `@see`로 관련 API 연결
