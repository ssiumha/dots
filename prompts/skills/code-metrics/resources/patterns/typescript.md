# TypeScript/JavaScript ast-grep Patterns

code-metrics에서 사용하는 ast-grep 패턴 정의.

## Complexity (복잡도)

### Cyclomatic Complexity 구성 요소

```bash
# if 문
ast-grep --lang typescript -p 'if ($COND) { $$$ }' <file> --json | jq length

# else if
ast-grep --lang typescript -p 'else if ($COND) { $$$ }' <file> --json | jq length

# for 문
ast-grep --lang typescript -p 'for ($INIT; $COND; $UPDATE) { $$$ }' <file> --json | jq length

# for...of
ast-grep --lang typescript -p 'for ($VAR of $ITER) { $$$ }' <file> --json | jq length

# for...in
ast-grep --lang typescript -p 'for ($VAR in $OBJ) { $$$ }' <file> --json | jq length

# while
ast-grep --lang typescript -p 'while ($COND) { $$$ }' <file> --json | jq length

# do...while
ast-grep --lang typescript -p 'do { $$$ } while ($COND)' <file> --json | jq length

# switch case
ast-grep --lang typescript -p 'case $VAL:' <file> --json | jq length

# ternary
ast-grep --lang typescript -p '$COND ? $THEN : $ELSE' <file> --json | jq length

# catch
ast-grep --lang typescript -p 'catch ($ERR) { $$$ }' <file> --json | jq length

# && (short-circuit) - 선택적
ast-grep --lang typescript -p '$LEFT && $RIGHT' <file> --json | jq length

# || (short-circuit) - 선택적
ast-grep --lang typescript -p '$LEFT || $RIGHT' <file> --json | jq length

# ?? (nullish coalescing) - 선택적
ast-grep --lang typescript -p '$LEFT ?? $RIGHT' <file> --json | jq length
```

**계산**: `CC = 1 + if + else_if + for + for_of + for_in + while + do_while + case + ternary + catch`

**주의 (논리 연산자)**: `&&`, `||`, `??`는 연쇄 시 중복 카운트 위험.
예: `a && b || c` → 2회 카운트 (과대평가). 기본 계산에서 제외 권장.

## Coupling (결합도)

### Efferent Coupling (Ce) - Import 수

```bash
# ES6 default import
ast-grep --lang typescript -p 'import $NAME from "$MOD"' <file> --json | jq length

# ES6 named import
ast-grep --lang typescript -p 'import { $$$ } from "$MOD"' <file> --json | jq length

# ES6 namespace import
ast-grep --lang typescript -p 'import * as $NAME from "$MOD"' <file> --json | jq length

# CommonJS require
ast-grep --lang typescript -p 'require("$MOD")' <file> --json | jq length

# Dynamic import
ast-grep --lang typescript -p 'import("$MOD")' <file> --json | jq length
```

**필터링**: 외부 모듈만 카운트 (상대 경로 `./`, `../` 제외 옵션)

## Call Graph (호출 관계)

### Fan-out - 함수 호출 수

```bash
# 일반 함수 호출
ast-grep --lang typescript -p '$FN($$$)' <file> --json | jq length

# 메서드 호출
ast-grep --lang typescript -p '$OBJ.$METHOD($$$)' <file> --json | jq length

# 체이닝 호출
ast-grep --lang typescript -p '$OBJ.$M1($$$).$M2($$$)' <file> --json | jq length
```

**주의**: 중복 카운트 가능 (체이닝). 근사치로 사용.

## Cohesion (응집도)

### 클래스 분석

**주의**: 클래스 내부 패턴은 중첩 중괄호로 인해 ast-grep 파싱이 어려울 수 있음.
대안으로 개별 요소 패턴 사용:

```bash
# 메서드 수 (클래스 내부 여부 무관)
ast-grep --lang typescript -p '$NAME($$$): $RET { $$$ }' <file> --json | jq length
ast-grep --lang typescript -p '$NAME($$$) { $$$ }' <file> --json | jq length

# 필드 선언 (타입 어노테이션 포함)
ast-grep --lang typescript -p '$FIELD: $TYPE;' <file> --json | jq length
ast-grep --lang typescript -p '$FIELD: $TYPE = $VAL;' <file> --json | jq length

# private 필드 (# prefix, ES2022)
ast-grep --lang typescript -p '#$FIELD = $VAL' <file> --json | jq length

# constructor 파라미터 (auto-assign)
ast-grep --lang typescript -p 'constructor($$$private $PARAM: $TYPE$$$)' <file> --json
ast-grep --lang typescript -p 'constructor($$$public $PARAM: $TYPE$$$)' <file> --json
```

**대안**: 클래스 단위 분석이 필요하면 TypeScript 컴파일러 API 사용 권장.

## Size (크기)

### 함수 크기

```bash
# 함수 선언
ast-grep --lang typescript -p 'function $NAME($$$) { $$$ }' <file> --json

# 화살표 함수
ast-grep --lang typescript -p 'const $NAME = ($$$) => { $$$ }' <file> --json

# 메서드
ast-grep --lang typescript -p '$NAME($$$) { $$$ }' <file> --json
```

## 통합 스크립트 예시

```bash
#!/bin/bash
FILE=$1

# Cyclomatic Complexity
IF=$(ast-grep --lang typescript -p 'if ($COND) { $$$ }' "$FILE" --json 2>/dev/null | jq length)
FOR=$(ast-grep --lang typescript -p 'for ($$$) { $$$ }' "$FILE" --json 2>/dev/null | jq length)
WHILE=$(ast-grep --lang typescript -p 'while ($COND) { $$$ }' "$FILE" --json 2>/dev/null | jq length)
CASE=$(ast-grep --lang typescript -p 'case $VAL:' "$FILE" --json 2>/dev/null | jq length)
TERNARY=$(ast-grep --lang typescript -p '$COND ? $THEN : $ELSE' "$FILE" --json 2>/dev/null | jq length)
CATCH=$(ast-grep --lang typescript -p 'catch ($ERR) { $$$ }' "$FILE" --json 2>/dev/null | jq length)

CC=$((1 + IF + FOR + WHILE + CASE + TERNARY + CATCH))

# Efferent Coupling
IMPORTS=$(ast-grep --lang typescript -p 'import $$$ from "$MOD"' "$FILE" --json 2>/dev/null | jq length)

# Fan-out
CALLS=$(ast-grep --lang typescript -p '$FN($$$)' "$FILE" --json 2>/dev/null | jq length)

# LOC
LOC=$(wc -l < "$FILE")

echo "LOC:$LOC CC:$CC Ce:$IMPORTS Fan:$CALLS"
```
