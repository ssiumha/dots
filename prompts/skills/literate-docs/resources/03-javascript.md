# JSDoc (JavaScript/TypeScript)

JavaScript와 TypeScript의 표준 문서화 규격입니다.

## 기본 규칙

- `/** ... */` 블록 주석 사용 (한 줄도 동일)
- 선언 바로 위에 위치
- 첫 줄은 요약 설명
- `@` 태그로 구조화

## 기본 형식

```javascript
/**
 * 사용자 정보를 가져온다.
 *
 * @param {string} userId - 사용자 ID
 * @returns {Promise<User>} 사용자 객체
 */
async function getUser(userId) {
  // ...
}
```

## 주요 태그

### 파라미터

```javascript
/**
 * @param {string} name - 이름
 * @param {number} [age] - 나이 (선택)
 * @param {Object} options - 옵션
 * @param {boolean} options.verbose - 상세 출력
 * @param {string} [options.format='json'] - 출력 형식
 */
```

### 반환값

```javascript
/**
 * @returns {boolean} 성공 여부
 * @returns {Promise<string>} 비동기 결과
 * @returns {Object} 결과 객체
 * @returns {Object.status} 상태 코드
 * @returns {Object.data} 데이터
 */
```

### 타입 정의

```javascript
/**
 * @typedef {Object} User
 * @property {string} id - 고유 ID
 * @property {string} name - 이름
 * @property {string} [email] - 이메일 (선택)
 */

/**
 * @typedef {'pending' | 'active' | 'inactive'} Status
 */
```

### 예외

```javascript
/**
 * @throws {TypeError} 인자가 문자열이 아닐 때
 * @throws {RangeError} 값이 범위를 벗어날 때
 */
```

### 예시

```javascript
/**
 * @example
 * // 기본 사용
 * const user = await getUser('123');
 *
 * @example
 * // 옵션 사용
 * const user = await getUser('123', { verbose: true });
 */
```

## 컨텍스트별 문서화

### 모듈/파일

```javascript
/**
 * @fileoverview HTTP 요청 유틸리티
 * @module utils/http
 * @author Alice <alice@example.com>
 *
 * HISTORY:
 *   2025-01-08: 초기 구현
 *   2025-01-15: 재시도 로직 추가
 */
```

### 클래스

```javascript
/**
 * 사용자를 나타내는 클래스.
 *
 * @class
 * @extends BaseEntity
 * @implements {Serializable}
 *
 * DECISION: class 대신 factory 패턴 검토
 *   - 이유: 의존성 주입 용이
 *   - 선택: class (IDE 지원, 타입 추론)
 */
class User extends BaseEntity {
  /**
   * User 인스턴스 생성
   *
   * @param {string} id - 고유 ID
   * @param {string} name - 이름
   */
  constructor(id, name) {
    // ...
  }
}
```

### 상수

```javascript
/**
 * 기본 타임아웃 (밀리초)
 *
 * WHY: 30초는 대부분의 API 응답 시간 커버
 * @constant {number}
 */
const DEFAULT_TIMEOUT = 30000;
```

### 콜백/함수 타입

```javascript
/**
 * @callback RequestCallback
 * @param {Error|null} error - 에러 객체
 * @param {Response} response - 응답 객체
 * @returns {void}
 */

/**
 * @param {RequestCallback} callback - 완료 콜백
 */
function request(callback) {
  // ...
}
```

## TypeScript 특화

### 제네릭

```typescript
/**
 * 배열에서 조건에 맞는 첫 요소를 찾는다.
 *
 * @template T
 * @param {T[]} array - 검색 대상 배열
 * @param {(item: T) => boolean} predicate - 조건 함수
 * @returns {T | undefined} 찾은 요소 또는 undefined
 */
function find<T>(array: T[], predicate: (item: T) => boolean): T | undefined {
  // ...
}
```

### 인터페이스

```typescript
/**
 * HTTP 요청 옵션
 *
 * WHY: fetch API 옵션과 호환성 유지
 */
interface RequestOptions {
  /** 요청 메서드 */
  method?: 'GET' | 'POST' | 'PUT' | 'DELETE';
  /** 요청 헤더 */
  headers?: Record<string, string>;
  /** 요청 본문 */
  body?: unknown;
  /**
   * 타임아웃 (밀리초)
   * @default 30000
   */
  timeout?: number;
}
```

### TSDoc 태그 (TypeScript 권장)

```typescript
/**
 * 데이터를 처리한다.
 *
 * @remarks
 * 이 함수는 내부적으로 캐싱을 사용한다.
 * 대용량 데이터에서는 스트리밍 API를 권장한다.
 *
 * @privateRemarks
 * 성능 최적화 검토 중 (JIRA-789)
 *
 * @beta
 */
```

## Codetags 통합

```javascript
/**
 * 검색을 수행한다.
 *
 * TODO(#123): 페이지네이션 추가
 * FIXME: 특수문자 검색 시 에러
 *
 * WHY: Elasticsearch 대신 PostgreSQL 전문검색 사용
 *   - 이유: 인프라 단순화, 동기화 불필요
 *   - 대안: Algolia (비용), MeiliSearch (운영 부담)
 *
 * @param {string} query - 검색어
 * @returns {Promise<SearchResult[]>} 검색 결과
 */
async function search(query) {
  // ...
}
```

## 도구 지원

### ESLint

```javascript
// eslint.config.js (flat config)
import jsdoc from 'eslint-plugin-jsdoc';

export default [
  {
    plugins: { jsdoc },
    rules: {
      'jsdoc/require-jsdoc': 'warn',
      'jsdoc/require-param-description': 'warn',
      'jsdoc/require-returns-description': 'warn',
    },
  },
];
```

### TypeDoc (문서 생성)

```bash
npx typedoc --entryPoints src/index.ts --out docs
```

### VS Code

`Document This` 확장으로 자동 생성 (Ctrl+Alt+D)

## Best Practices

1. **타입 힌트 우선**: TypeScript는 타입 주석보다 타입 선언
2. **설명 중심**: `@param {string} name` 대신 `@param name - 사용자 이름`
3. **예시 필수**: 복잡한 API는 `@example` 포함
4. **WHY 기록**: 라이브러리 선택, 패턴 결정 시 이유 명시
