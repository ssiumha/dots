#!/bin/bash
# lib/frontmatter.sh - YAML frontmatter 파싱 함수

# frontmatter 추출 (yq 사용)
# Usage: get_frontmatter <file>
get_frontmatter() {
  local file="$1"
  yq --front-matter=extract '.' "$file" 2>/dev/null
}

# frontmatter에서 특정 필드 값 추출
# Usage: get_field <file> <field>
get_field() {
  local file="$1"
  local field="$2"
  yq --front-matter=extract ".${field}" "$file" 2>/dev/null | grep -v '^null$'
}

# frontmatter에서 배열 필드를 줄별로 출력
# Usage: get_array_field <file> <field>
get_array_field() {
  local file="$1"
  local field="$2"
  yq --front-matter=extract ".${field}[]" "$file" 2>/dev/null
}

# TODO 파일인지 확인
is_todo_file() {
  local file="$1"
  local type
  type=$(get_field "$file" "type")
  [ "$type" = "todo" ]
}

# 상태 정규화 (in_progress → in-progress)
normalize_status() {
  local status="$1"
  case "$status" in
    in_progress) echo "in-progress" ;;
    *)           echo "$status" ;;
  esac
}

# TODO 상태 확인 (기본값: pending, 정규화 적용)
get_todo_status() {
  local file="$1"
  local status
  status=$(get_field "$file" "status")
  status="${status:-pending}"
  normalize_status "$status"
}

# TODO 우선순위 확인
get_todo_priority() {
  local file="$1"
  local priority
  priority=$(get_field "$file" "priority")
  echo "${priority:-medium}"
}

# TODO 생성일 확인
get_todo_created() {
  local file="$1"
  get_field "$file" "created"
}

# TODO 마감일 확인
get_todo_deadline() {
  local file="$1"
  get_field "$file" "deadline"
}

# TODO 완료일 확인
get_todo_completed() {
  local file="$1"
  get_field "$file" "completed"
}

# TODO ID 확인
get_todo_id() {
  local file="$1"
  get_field "$file" "id"
}

# 파일 제목 추출 (첫 번째 # 헤더)
get_title() {
  local file="$1"
  grep -m1 '^# ' "$file" 2>/dev/null | sed 's/^# //'
}

# 파일 태그 목록 추출
get_tags() {
  local file="$1"
  get_array_field "$file" "tags" 2>/dev/null | tr '\n' ',' | sed 's/,$//'
}

# 우선순위를 정렬 가능한 숫자로 변환
priority_to_num() {
  local priority="$1"
  case "$priority" in
    critical) echo 0 ;;
    urgent)   echo 1 ;;
    high)     echo 2 ;;
    medium)   echo 3 ;;
    low)      echo 4 ;;
    *)        echo 5 ;;
  esac
}

# 우선순위 숫자를 문자로 변환
num_to_priority() {
  local num="$1"
  case "$num" in
    1) echo "urgent" ;;
    2) echo "high" ;;
    3) echo "medium" ;;
    4) echo "low" ;;
    *) echo "unknown" ;;
  esac
}

# 상태별 TODO 파일 목록 (빠른 선필터링 with rg)
# Usage: list_todos_by_status <todos_dir> <status>
list_todos_by_status() {
  local todos_dir="$1"
  local status="$2"

  if [ -z "$status" ] || [ "$status" = "all" ]; then
    # 모든 TODO (completed 제외)
    find "$todos_dir" -maxdepth 1 -name "*.md" -type f 2>/dev/null
  else
    # 특정 상태만
    rg -l "^status: ${status}$" "$todos_dir"/*.md 2>/dev/null
  fi
}

# 완료된 TODO 파일 목록 (completed/ 하위)
list_completed_todos() {
  local todos_dir="$1"
  find "${todos_dir}/completed" -name "*.md" -type f 2>/dev/null
}

# TODO 정보를 파이프 구분 형식으로 출력
# Format: status|priority|created|deadline|id|title|file
format_todo_info() {
  local file="$1"

  local status priority created deadline id title

  status=$(get_todo_status "$file")
  priority=$(get_todo_priority "$file")
  created=$(get_todo_created "$file")
  deadline=$(get_todo_deadline "$file")
  id=$(get_todo_id "$file")
  title=$(get_title "$file")

  echo "${status}|${priority}|${created}|${deadline:-none}|${id}|${title}|${file}"
}
