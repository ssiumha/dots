---
description: Jest/Vitest 문법 레퍼런스
paths:
  - "**/*.test.ts"
  - "**/*.spec.ts"
  - "**/*.test.tsx"
  - "**/*.spec.tsx"
---

Jest/Vitest 도구/문법 레퍼런스입니다. 테스트 원칙은 `/test-guidelines` 참조.

## 테스트 구조

```typescript
describe('모듈명', () => {
  beforeEach(() => { /* 각 테스트 전 실행 */ });
  afterEach(() => { /* 각 테스트 후 정리 */ });

  it('동작 설명', () => { expect(result).toBe(expected); });
});
```

## test.each 패턴

```typescript
test.each([
  [1, 2, 3],
  [2, 3, 5],
])('add(%i, %i) = %i', (a, b, expected) => {
  expect(add(a, b)).toBe(expected);
});

test.each([
  { input: 'a', expected: 'A' },
  { input: 'b', expected: 'B' },
])('toUpper($input) = $expected', ({ input, expected }) => {
  expect(toUpper(input)).toBe(expected);
});
```

## Mock

```typescript
// Jest
const mockFn = jest.fn().mockReturnValue(42);
jest.mock('./module', () => ({ fn: jest.fn() }));
jest.spyOn(object, 'method').mockImplementation(() => 'mocked');

// Vitest (vi 사용)
const mockFn = vi.fn().mockReturnValue(42);
vi.mock('./module', () => ({ fn: vi.fn() }));
vi.spyOn(object, 'method').mockImplementation(() => 'mocked');
```

## 주요 매처

| 매처 | 용도 |
|------|------|
| `toBe(v)` | 원시값 동등 (===) |
| `toEqual(v)` | 객체/배열 깊은 비교 |
| `toMatchObject(v)` | 객체 부분 일치 |
| `toThrow(e?)` | 예외 발생 |
| `toHaveBeenCalledWith(...)` | mock 호출 인자 |
| `resolves/rejects` | Promise 검증 |
