#!/bin/bash
# code-metrics: 코드 구조 메트릭 측정
# Usage: ./measure.sh <file> [--json]

set -euo pipefail

FILE="${1:-}"
FORMAT="${2:-text}"

if [[ -z "$FILE" || ! -f "$FILE" ]]; then
    echo "Usage: $0 <file> [--json]" >&2
    exit 1
fi

# 언어 감지
detect_lang() {
    case "${1##*.}" in
        ts|tsx|js|jsx|mjs|cjs) echo "typescript" ;;
        py|pyw) echo "python" ;;
        go) echo "go" ;;
        rs) echo "rust" ;;
        *) echo "unknown" ;;
    esac
}

LANG=$(detect_lang "$FILE")

# ast-grep 카운트 헬퍼
count_pattern() {
    local lang=$1
    local pattern=$2
    local file=$3
    local result

    result=$(ast-grep --lang "$lang" -p "$pattern" "$file" --json 2>/dev/null) || { echo 0; return; }
    # 빈 문자열 또는 null 체크
    [[ -z "$result" || "$result" == "null" ]] && { echo 0; return; }
    echo "$result" | jq 'if type == "array" then length else 0 end' 2>/dev/null || echo 0
}

# LOC 측정
measure_loc() {
    wc -l < "$1" | tr -d ' '
}

# 들여쓰기 단위 감지
detect_indent_size() {
    local file=$1
    # 첫 번째 들여쓰기된 라인의 스페이스 수
    local first_indent
    first_indent=$(grep -o '^[[:space:]]*' "$file" 2>/dev/null | grep -v '^$' | awk '{print length}' | sort -n | grep -v '^0$' | head -1)
    echo "${first_indent:-2}"
}

# 최대 중첩 깊이 (들여쓰기 기반, 근사)
measure_nesting() {
    local file=$1
    local max_indent=0

    # 탭/스페이스 비율 확인 후 우세한 방식 선택
    local tab_lines space_lines
    tab_lines=$(grep -c $'^\t' "$file" 2>/dev/null | tr -d '[:space:]' || echo 0)
    space_lines=$(grep -c '^ ' "$file" 2>/dev/null | tr -d '[:space:]' || echo 0)
    # 숫자가 아니면 0으로
    [[ ! "$tab_lines" =~ ^[0-9]+$ ]] && tab_lines=0
    [[ ! "$space_lines" =~ ^[0-9]+$ ]] && space_lines=0

    if [[ $tab_lines -gt $space_lines ]]; then
        # 탭 기준
        max_indent=$(awk -F'\t' '{print NF-1}' "$file" 2>/dev/null | sort -rn | head -1)
    else
        # 스페이스 기준 (동적 감지)
        local indent_size
        indent_size=$(detect_indent_size "$file")
        max_indent=$(awk -v size="$indent_size" '{match($0, /^[ ]*/); print int(RLENGTH/size)}' "$file" 2>/dev/null | sort -rn | head -1)
    fi

    echo "${max_indent:-0}"
}

# TypeScript 메트릭
measure_typescript() {
    local file=$1

    # Cyclomatic Complexity
    local if_count=$(count_pattern typescript 'if ($COND) { $$$ }' "$file")
    local for_count=$(count_pattern typescript 'for ($$$) { $$$ }' "$file")
    local while_count=$(count_pattern typescript 'while ($COND) { $$$ }' "$file")
    local case_count=$(count_pattern typescript 'case $VAL:' "$file")
    local ternary=$(count_pattern typescript '$COND ? $THEN : $ELSE' "$file")
    local catch_count=$(count_pattern typescript 'catch ($ERR) { $$$ }' "$file")

    local cc=$((1 + if_count + for_count + while_count + case_count + ternary + catch_count))

    # Efferent Coupling (Ce) - imports
    local import_default=$(count_pattern typescript 'import $NAME from "$MOD"' "$file")
    local import_named=$(count_pattern typescript 'import { $$$ } from "$MOD"' "$file")
    local import_ns=$(count_pattern typescript 'import * as $NAME from "$MOD"' "$file")
    local require_count=$(count_pattern typescript 'require("$MOD")' "$file")

    local ce=$((import_default + import_named + import_ns + require_count))

    # Fan-out (함수 호출)
    local calls=$(count_pattern typescript '$FN($$$)' "$file")

    echo "$cc $ce $calls"
}

# Python 메트릭
measure_python() {
    local file=$1

    # Cyclomatic Complexity
    local if_count=$(count_pattern python 'if $COND: $$$' "$file")
    local elif_count=$(count_pattern python 'elif $COND: $$$' "$file")
    local for_count=$(count_pattern python 'for $VAR in $ITER: $$$' "$file")
    local while_count=$(count_pattern python 'while $COND: $$$' "$file")
    local except_count=$(count_pattern python 'except $$$: $$$' "$file")
    local ternary=$(count_pattern python '$T if $C else $E' "$file")

    local cc=$((1 + if_count + elif_count + for_count + while_count + except_count + ternary))

    # Efferent Coupling (Ce) - imports
    local import_count=$(count_pattern python 'import $MOD' "$file")
    local from_count=$(count_pattern python 'from $MOD import $$$' "$file")

    local ce=$((import_count + from_count))

    # Fan-out (함수 호출)
    local calls=$(count_pattern python '$FN($$$)' "$file")

    echo "$cc $ce $calls"
}

# 메트릭 측정
LOC=$(measure_loc "$FILE")
NESTING=$(measure_nesting "$FILE")

# 기본값 초기화
CC=0
CE=0
FANOUT=0

case "$LANG" in
    typescript)
        RESULT=$(measure_typescript "$FILE") || true
        if [[ -n "$RESULT" ]]; then
            read CC CE FANOUT <<< "$RESULT"
        fi
        ;;
    python)
        RESULT=$(measure_python "$FILE") || true
        if [[ -n "$RESULT" ]]; then
            read CC CE FANOUT <<< "$RESULT"
        fi
        ;;
esac

# 출력
if [[ "$FORMAT" == "--json" ]]; then
    cat <<EOF
{
  "file": "$FILE",
  "lang": "$LANG",
  "loc": $LOC,
  "cyclomatic": $CC,
  "nesting": $NESTING,
  "efferent_coupling": $CE,
  "fanout": $FANOUT
}
EOF
else
    echo "File: $FILE"
    echo "Lang: $LANG"
    echo "LOC: $LOC"
    echo "Cyclomatic: $CC"
    echo "Nesting: $NESTING"
    echo "Ce: $CE"
    echo "Fan-out: $FANOUT"
fi
