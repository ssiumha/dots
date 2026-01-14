# Python ast-grep Patterns

code-metrics에서 사용하는 ast-grep 패턴 정의 (Python).

## Complexity (복잡도)

### Cyclomatic Complexity 구성 요소

```bash
# if 문
ast-grep --lang python -p 'if $COND: $$$' <file> --json | jq length

# elif
ast-grep --lang python -p 'elif $COND: $$$' <file> --json | jq length

# for 문
ast-grep --lang python -p 'for $VAR in $ITER: $$$' <file> --json | jq length

# while
ast-grep --lang python -p 'while $COND: $$$' <file> --json | jq length

# except (catch)
ast-grep --lang python -p 'except $EXC: $$$' <file> --json | jq length
ast-grep --lang python -p 'except: $$$' <file> --json | jq length

# ternary (conditional expression)
ast-grep --lang python -p '$THEN if $COND else $ELSE' <file> --json | jq length

# list comprehension with condition
ast-grep --lang python -p '[$EXPR for $VAR in $ITER if $COND]' <file> --json | jq length

# and (short-circuit)
ast-grep --lang python -p '$LEFT and $RIGHT' <file> --json | jq length

# or (short-circuit)
ast-grep --lang python -p '$LEFT or $RIGHT' <file> --json | jq length

# match case (Python 3.10+)
ast-grep --lang python -p 'case $PATTERN: $$$' <file> --json | jq length
```

**계산**: `CC = 1 + if + elif + for + while + except + ternary + comprehension_if + (and/or 선택) + case`

## Coupling (결합도)

### Efferent Coupling (Ce) - Import 수

```bash
# import 모듈
ast-grep --lang python -p 'import $MOD' <file> --json | jq length

# from import
ast-grep --lang python -p 'from $MOD import $$$' <file> --json | jq length

# import as
ast-grep --lang python -p 'import $MOD as $ALIAS' <file> --json | jq length

# from import as
ast-grep --lang python -p 'from $MOD import $NAME as $ALIAS' <file> --json | jq length
```

**필터링**:
- 표준 라이브러리 제외 옵션
- 상대 import (`.`, `..`) 별도 카운트

## Call Graph (호출 관계)

### Fan-out - 함수 호출 수

```bash
# 일반 함수 호출
ast-grep --lang python -p '$FN($$$)' <file> --json | jq length

# 메서드 호출
ast-grep --lang python -p '$OBJ.$METHOD($$$)' <file> --json | jq length

# 체이닝 호출
ast-grep --lang python -p '$OBJ.$M1($$$).$M2($$$)' <file> --json | jq length
```

## Cohesion (응집도)

### 클래스 분석

**주의**: 클래스 내부 패턴은 ast-grep에서 중첩 구조로 인해 파싱이 어려울 수 있음.

```bash
# 인스턴스 메서드 (self 파라미터)
ast-grep --lang python -p 'def $METHOD(self, $$$): $$$' <file> --json | jq length
ast-grep --lang python -p 'def $METHOD(self): $$$' <file> --json | jq length

# 정적/클래스 메서드 - 데코레이터 개별 카운트 (개행 문제 우회)
ast-grep --lang python -p '@staticmethod' <file> --json | jq length
ast-grep --lang python -p '@classmethod' <file> --json | jq length

# 클래스 속성 (self.X = 패턴)
ast-grep --lang python -p 'self.$ATTR = $VAL' <file> --json | jq 'unique_by(.text | split("=")[0])' | jq length
```

**대안 (데코레이터 + 함수)**: 개행 포함 패턴은 YAML 규칙 파일 사용 권장.
```yaml
# rules/python-methods.yaml
rules:
  - id: staticmethod
    pattern: |
      @staticmethod
      def $METHOD($$$): $$$
    language: python
```

## Size (크기)

### 함수 크기

```bash
# 함수 정의
ast-grep --lang python -p 'def $NAME($$$): $$$' <file> --json

# async 함수
ast-grep --lang python -p 'async def $NAME($$$): $$$' <file> --json

# lambda
ast-grep --lang python -p 'lambda $$$: $EXPR' <file> --json | jq length
```

### 파라미터 수

```bash
# 함수 파라미터 (근사) - 정확한 카운트는 어려움
ast-grep --lang python -p 'def $NAME($PARAM): $$$' <file> --json   # 1개
ast-grep --lang python -p 'def $NAME($P1, $P2): $$$' <file> --json  # 2개
# 가변 인자는 별도 처리 필요
```

## 데코레이터 분석 (보너스)

```bash
# 데코레이터 카운트
ast-grep --lang python -p '@$DEC
def $NAME($$$): $$$' <file> --json | jq length

# 특정 데코레이터
ast-grep --lang python -p '@property
def $NAME(self): $$$' <file> --json | jq length
```

## 통합 스크립트 예시

```bash
#!/bin/bash
FILE=$1

# Cyclomatic Complexity
IF=$(ast-grep --lang python -p 'if $COND: $$$' "$FILE" --json 2>/dev/null | jq length)
ELIF=$(ast-grep --lang python -p 'elif $COND: $$$' "$FILE" --json 2>/dev/null | jq length)
FOR=$(ast-grep --lang python -p 'for $VAR in $ITER: $$$' "$FILE" --json 2>/dev/null | jq length)
WHILE=$(ast-grep --lang python -p 'while $COND: $$$' "$FILE" --json 2>/dev/null | jq length)
EXCEPT=$(ast-grep --lang python -p 'except $$$: $$$' "$FILE" --json 2>/dev/null | jq length)
TERNARY=$(ast-grep --lang python -p '$T if $C else $E' "$FILE" --json 2>/dev/null | jq length)

CC=$((1 + IF + ELIF + FOR + WHILE + EXCEPT + TERNARY))

# Efferent Coupling
IMPORT=$(ast-grep --lang python -p 'import $MOD' "$FILE" --json 2>/dev/null | jq length)
FROM=$(ast-grep --lang python -p 'from $MOD import $$$' "$FILE" --json 2>/dev/null | jq length)
CE=$((IMPORT + FROM))

# Fan-out
CALLS=$(ast-grep --lang python -p '$FN($$$)' "$FILE" --json 2>/dev/null | jq length)

# LOC
LOC=$(wc -l < "$FILE")

echo "LOC:$LOC CC:$CC Ce:$CE Fan:$CALLS"
```

## 제한사항

1. **동적 타이핑**: 메서드 호출 vs 함수 호출 구분 어려움
2. **메타클래스**: 복잡한 클래스 구조 분석 한계
3. **데코레이터**: 동작 변경 추적 불가
4. **컴프리헨션**: 중첩 컴프리헨션 복잡도 과소평가 가능
