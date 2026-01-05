# 경계값 테스트 가이드 (Boundary Value Analysis)

## 왜 중요한가

- Off-by-one 오류 방지 (가장 흔한 버그)
- 날짜/시간 경계 이슈 방지
- 빈 값, null, 극단값 처리 검증

## 경계값 테스트 체크리스트

### 숫자 (Numeric)

| 경계 | 테스트 값 |
|------|----------|
| 최소값 | min, min-1, min+1 |
| 최대값 | max, max-1, max+1 |
| 0 경계 | -1, 0, 1 |
| 범위 | range 시작, 끝, 바로 밖 |

예: 나이 검증 (0-150)
→ -1, 0, 1, 149, 150, 151

### 문자열 (String)

| 경계 | 테스트 값 |
|------|----------|
| 빈 값 | "", null, undefined |
| 길이 | 최소 길이, 최대 길이, 초과 |
| 특수 | 공백만, 유니코드, 이모지 |

예: 사용자명 검증 (2-20자)
→ "", " ", "a", "ab", "a"*20, "a"*21

### 컬렉션 (Array/List)

| 경계 | 테스트 값 |
|------|----------|
| 빈 컬렉션 | [], {} |
| 단일 요소 | [1] |
| 다수 요소 | [1, 2, 3, ...] |
| 인덱스 | 0, length-1, length (out of bounds) |

예: 평균 계산 함수
→ [], [5], [1,2,3], [MAX_INT, MAX_INT]

### 날짜/시간 경계

| 경계 | 테스트 값 |
|------|----------|
| 월말 | 28, 29, 30, 31일 |
| 윤년 | 2/28, 2/29 |
| 연말 | 12/31, 1/1 |
| 자정 | 23:59:59, 00:00:00 |
| 타임존 | UTC, 로컬, DST 전환 |

예: 구독 만료 체크
→ 만료일 전날, 당일 00:00, 당일 23:59, 다음날

### 페이지네이션

| 경계 | 테스트 값 |
|------|----------|
| 첫 페이지 | page=0 or 1 |
| 마지막 | 정확히 마지막, 초과 |
| 페이지 크기 | size=0, 1, max, max+1 |

### Boolean/Flag

| 경계 | 테스트 값 |
|------|----------|
| 참/거짓 | true, false |
| Falsy | null, undefined, 0, "", false |
| Truthy | 1, "true", [], {} |

---

## Off-by-one 오류 패턴

```python
# ❌ 흔한 실수
for i in range(len(arr)):       # 0 ~ len-1
    arr[i+1]                     # IndexError at last

# ❌ 흔한 실수
if end_date >= today:           # 당일 포함?
    return "active"

# ✅ 명확하게
if end_date > today:            # 또는 >= today 의도적으로
    return "active"
```

```javascript
// ❌ 흔한 실수
for (let i = 0; i <= arr.length; i++)  // <= 는 out of bounds

// ✅ 올바름
for (let i = 0; i < arr.length; i++)
```

---

## 날짜 테스트 패턴

### Python (freezegun)

```python
from freezegun import freeze_time

@freeze_time("2024-02-29 23:59:59")
def test_leap_year_boundary():
    assert is_leap_year(2024)

@freeze_time("2024-12-31 23:59:59")
def test_year_end_boundary():
    assert get_next_day().year == 2025
```

### JavaScript (jest)

```javascript
beforeEach(() => {
  jest.useFakeTimers();
  jest.setSystemTime(new Date('2024-02-29T23:59:59Z'));
});

afterEach(() => {
  jest.useRealTimers();
});
```

### Java (Mockito)

```java
@Mock
private Clock clock;

@Test
void testLeapYearBoundary() {
    when(clock.instant()).thenReturn(
        Instant.parse("2024-02-29T23:59:59Z")
    );
    // ...
}
```

### Go (testify)

```go
func TestLeapYearBoundary(t *testing.T) {
    // 시간 고정
    fixedTime := time.Date(2024, 2, 29, 23, 59, 59, 0, time.UTC)

    result := IsLeapYear(fixedTime)
    assert.True(t, result)
}
```

---

## 테스트 케이스 생성 규칙

1. **경계 ±1 규칙**: 경계값과 양쪽 1씩
2. **Equivalence Partitioning**: 동등 클래스 대표값
3. **특수값**: null, 빈값, 최대값
4. **조합**: 복수 경계가 만나는 지점

### 예시: 할인율 계산 (0-100%)

```text
유효 범위: 0 <= discount <= 100

테스트 케이스:
- 경계: -1, 0, 1, 99, 100, 101
- 대표값: 50 (중간)
- 특수: null, undefined, "50%"
```

---

## 흔히 놓치는 경계

| 케이스 | 설명 |
|--------|------|
| 빈 입력 | 빈 문자열, 빈 배열, null |
| 단일 요소 | 배열에 1개만 있을 때 |
| 동일 값 | start == end, min == max |
| 정수 오버플로우 | MAX_INT + 1 |
| 부동소수점 | 0.1 + 0.2 != 0.3 |
| 유니코드 | 이모지, 한글, RTL 문자 |
| 시간대 | DST 전환, 타임존 변경 |
