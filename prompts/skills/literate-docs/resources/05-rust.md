# rustdoc (Rust)

Rust 언어의 공식 문서화 규격입니다.

## 기본 규칙

- `///` 외부 문서 (pub 항목)
- `//!` 내부 문서 (모듈/크레이트)
- Markdown 지원
- 첫 줄은 요약

## 기본 형식

```rust
/// Fetches data from the given URL.
///
/// Returns the response body as bytes.
///
/// # Errors
///
/// Returns an error if the request fails.
pub fn fetch_data(url: &str) -> Result<Vec<u8>, Error> {
    // ...
}
```

## 주요 섹션

### # Examples (필수 권장)

```rust
/// Parses a string into an integer.
///
/// # Examples
///
/// ```
/// let n = mylib::parse_int("42")?;
/// assert_eq!(n, 42);
/// ```
///
/// Negative numbers are supported:
///
/// ```
/// let n = mylib::parse_int("-10")?;
/// assert_eq!(n, -10);
/// ```
pub fn parse_int(s: &str) -> Result<i32, ParseError> {
    // ...
}
```

### # Errors

```rust
/// Reads the file at the given path.
///
/// # Errors
///
/// This function will return an error if:
/// - The file does not exist
/// - The user lacks permission to read the file
/// - The file contains invalid UTF-8
pub fn read_file(path: &Path) -> Result<String, io::Error> {
    // ...
}
```

### # Panics

```rust
/// Divides two numbers.
///
/// # Panics
///
/// Panics if `divisor` is zero.
///
/// # Examples
///
/// ```
/// assert_eq!(divide(10, 2), 5);
/// ```
pub fn divide(dividend: i32, divisor: i32) -> i32 {
    // ...
}
```

### # Safety (unsafe 함수 필수)

```rust
/// Dereferences a raw pointer.
///
/// # Safety
///
/// The caller must ensure that:
/// - `ptr` is valid and properly aligned
/// - `ptr` points to an initialized value of type `T`
/// - The value is not being mutated by other code
pub unsafe fn deref_ptr<T>(ptr: *const T) -> T {
    // ...
}
```

## 컨텍스트별 문서화

### 크레이트/모듈 (내부 문서)

```rust
//! # MyLib
//!
//! A library for processing data efficiently.
//!
//! ## Features
//!
//! - Fast parsing
//! - Zero-copy operations
//! - Async support
//!
//! ## Quick Start
//!
//! ```rust
//! use mylib::process;
//!
//! let result = process("input data")?;
//! println!("{}", result);
//! ```
//!
//! HISTORY:
//!   2025-01-08: 초기 릴리즈
//!   2025-01-15: async 지원 추가
```

### 구조체

```rust
/// A user in the system.
///
/// # DECISION: `String` vs `&str`
///
/// 소유권 명확화를 위해 `String` 사용.
/// - 이유: 라이프타임 복잡도 감소
/// - 대안: `Cow<'a, str>` (유연하지만 복잡)
#[derive(Debug, Clone)]
pub struct User {
    /// The unique identifier.
    pub id: String,

    /// The display name.
    pub name: String,

    /// The email address, if provided.
    pub email: Option<String>,
}
```

### Enum

```rust
/// The status of a task.
///
/// WHY: u8 대신 enum 사용
///   - 이유: 타입 안전성, 패턴 매칭
///   - 대안: bitflags (복합 상태 필요 시)
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Status {
    /// The task is waiting to be processed.
    Pending,
    /// The task is currently running.
    Running,
    /// The task has completed successfully.
    Completed,
    /// The task has failed with an error.
    Failed,
}
```

### Trait

```rust
/// A type that can be serialized to bytes.
///
/// # Examples
///
/// ```
/// struct MyData { value: i32 }
///
/// impl Serialize for MyData {
///     fn serialize(&self) -> Vec<u8> {
///         self.value.to_le_bytes().to_vec()
///     }
/// }
/// ```
pub trait Serialize {
    /// Serializes the value to bytes.
    fn serialize(&self) -> Vec<u8>;
}
```

### impl 블록

```rust
impl User {
    /// Creates a new user with the given ID and name.
    ///
    /// # Examples
    ///
    /// ```
    /// let user = User::new("1", "Alice");
    /// assert_eq!(user.name, "Alice");
    /// ```
    pub fn new(id: impl Into<String>, name: impl Into<String>) -> Self {
        // ...
    }

    /// Returns the user's display name.
    ///
    /// If the email is set, returns "name <email>".
    /// Otherwise, returns just the name.
    pub fn display_name(&self) -> String {
        // ...
    }
}
```

## 고급 기능

### 링크

```rust
/// Parses configuration from a [`Config`] file.
///
/// See also [`parse_json`] and [`parse_toml`].
///
/// For more details, see the [configuration guide].
///
/// [`Config`]: crate::config::Config
/// [configuration guide]: https://example.com/docs/config
pub fn parse_config() -> Config {
    // ...
}
```

### 코드 블록 속성

```rust
/// # Examples
///
/// This example should compile but not run:
/// ```no_run
/// let server = Server::bind("0.0.0.0:8080")?;
/// server.run();  // Blocks forever
/// ```
///
/// This example should fail to compile:
/// ```compile_fail
/// let x: i32 = "not a number";
/// ```
///
/// This is just for illustration:
/// ```ignore
/// // Platform-specific code
/// ```
```

### Feature 게이팅

```rust
/// Async version of [`fetch_data`].
///
/// Requires the `async` feature.
///
/// # Examples
///
/// ```
/// # #[cfg(feature = "async")]
/// # async fn example() -> Result<(), Error> {
/// let data = fetch_data_async("https://example.com").await?;
/// # Ok(())
/// # }
/// ```
#[cfg(feature = "async")]
pub async fn fetch_data_async(url: &str) -> Result<Vec<u8>, Error> {
    // ...
}
```

## Codetags 통합

```rust
/// Processes an order.
///
/// TODO(#123): 배치 처리 지원
/// FIXME: 대용량 주문 시 스택 오버플로우
///
/// # WHY: `async` 대신 `sync`
///
/// - 이유: 대부분의 사용 사례가 동기적
/// - 대안: async (tokio 의존성 추가)
/// - 벤치마크: benches/order_processing.rs
///
/// # DECISION: `Box<dyn Error>` 사용
///
/// - Context: 다양한 에러 타입 통합 필요
/// - Options:
///   1. Custom Error enum (타입 안전, 보일러플레이트)
///   2. anyhow::Error (유연, 타입 정보 손실)
///   3. Box<dyn Error> (표준, 중간 수준)
/// - Choice: Box<dyn Error>
/// - Consequence: 다운캐스팅 필요 시 복잡
pub fn process_order(order: &Order) -> Result<Receipt, Box<dyn Error>> {
    // ...
}
```

## Deprecated

```rust
/// Old function for processing data.
///
/// # Deprecated
///
/// Use [`process_v2`] instead. This function will be removed in 2.0.
#[deprecated(since = "1.5.0", note = "Use `process_v2` instead")]
pub fn process(data: &[u8]) -> Vec<u8> {
    // ...
}
```

## 도구 지원

### cargo doc

```bash
# 문서 생성
cargo doc --open

# 비공개 항목 포함
cargo doc --document-private-items

# 의존성 제외
cargo doc --no-deps
```

### rustdoc 테스트

```bash
# 문서 내 코드 예시 테스트
cargo test --doc
```

### clippy 린트

```bash
# 문서 관련 린트
cargo clippy -- -W missing_docs -W missing_doc_code_examples
```

## Best Practices

1. **Examples 필수**: 모든 pub 함수에 예시 코드
2. **Errors/Panics 명시**: 실패 조건 문서화
3. **Safety 필수**: unsafe 함수는 반드시 Safety 섹션
4. **링크 활용**: 관련 타입/함수 상호 참조
5. **doc test 실행**: 예시 코드가 실제로 동작하는지 확인
