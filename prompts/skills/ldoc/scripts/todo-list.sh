#!/bin/bash
# todo-list.sh - TODO 목록 조회

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

# shellcheck source=lib/utils.sh
source "${SCRIPT_DIR}/lib/utils.sh"
# shellcheck source=lib/frontmatter.sh
source "${SCRIPT_DIR}/lib/frontmatter.sh"

usage() {
  cat << EOF
Usage: $(basename "$0") [options]

TODO 목록을 조회합니다.

Options:
  -p, --project PROJECT   프로젝트명 (기본: 현재 디렉토리에서 추론)
  -s, --status STATUS     상태 필터 (pending|in-progress|done|all)
  --priority PRIORITY     우선순위 필터 (urgent|high|medium|low)
  --summary               요약만 출력 (개수만)
  --json                  JSON 형식으로 출력
  --plain                 탭 구분 플레인 텍스트
  -h, --help              도움말

Examples:
  $(basename "$0")                    # 현재 프로젝트 TODO 전체
  $(basename "$0") -s pending         # 대기 중인 TODO만
  $(basename "$0") --priority urgent  # 긴급 TODO만
  $(basename "$0") --summary          # 요약만 출력
EOF
}

# 기본값
PROJECT=""
STATUS=""
PRIORITY=""
SUMMARY=false
OUTPUT_FORMAT="human"

# 옵션 파싱
while [[ $# -gt 0 ]]; do
  case $1 in
    -p|--project)
      PROJECT="$2"
      shift 2
      ;;
    -s|--status)
      STATUS="$2"
      shift 2
      ;;
    --priority)
      PRIORITY="$2"
      shift 2
      ;;
    --summary)
      SUMMARY=true
      shift
      ;;
    --json)
      OUTPUT_FORMAT="json"
      shift
      ;;
    --plain)
      OUTPUT_FORMAT="plain"
      shift
      ;;
    -h|--help)
      usage
      exit 0
      ;;
    *)
      error "Unknown option: $1"
      usage
      exit 1
      ;;
  esac
done

# 의존성 확인
check_required_deps || exit 1

# 프로젝트 감지
if [ -z "$PROJECT" ]; then
  PROJECT=$(detect_project)
  if [ -z "$PROJECT" ]; then
    error "Cannot detect project. Use -p option."
    exit 1
  fi
fi

# 문서 디렉토리 확인
DOCS_DIR=$(get_docs_dir "$PROJECT")
TODOS_DIR=$(get_todos_dir "$PROJECT")

if [ ! -d "$TODOS_DIR" ]; then
  error "TODO directory not found: $TODOS_DIR"
  exit 1
fi

# TODO 수집
declare -a TODOS=()

while IFS= read -r file; do
  [ -f "$file" ] || continue

  # 상태 필터링
  file_status=$(get_todo_status "$file" || echo "")
  [ -z "$file_status" ] && continue  # 상태가 없으면 건너뜀

  if [ -n "$STATUS" ] && [ "$STATUS" != "all" ] && [ "$file_status" != "$STATUS" ]; then
    continue
  fi

  # 우선순위 필터링
  if [ -n "$PRIORITY" ]; then
    file_priority=$(get_todo_priority "$file" || echo "medium")
    if [ "$file_priority" != "$PRIORITY" ]; then
      continue
    fi
  fi

  # TODO 정보 수집
  todo_info=$(format_todo_info "$file" || echo "")
  [ -n "$todo_info" ] && TODOS+=("$todo_info")
done < <(find "$TODOS_DIR" -maxdepth 1 -name "*.md" -type f 2>/dev/null)

# 빈 배열 체크
if [ ${#TODOS[@]} -eq 0 ]; then
  case "$OUTPUT_FORMAT" in
    json)
      echo "[]"
      ;;
    plain)
      # 아무 출력 없음
      ;;
    human)
      if [ "$SUMMARY" = true ]; then
        echo "[${PROJECT}] TODO: 0 pending, 0 in-progress"
      else
        echo ""
        echo "[${PROJECT}] TODO 현황"
        print_separator 70
        info "No TODOs found."
      fi
      ;;
  esac
  exit 0
fi

# 정렬 (우선순위 → 생성일)
IFS=$'\n' SORTED_TODOS=($(
  for todo in "${TODOS[@]}"; do
    priority=$(echo "$todo" | cut -d'|' -f2)
    priority_num=$(priority_to_num "$priority")
    echo "${priority_num}|${todo}"
  done | sort -t'|' -k1,1n -k4,4 | cut -d'|' -f2-
))
unset IFS

# 통계 계산
count_pending=0
count_in_progress=0
count_done=0

for todo in "${SORTED_TODOS[@]+"${SORTED_TODOS[@]}"}"; do
  [ -z "$todo" ] && continue
  status=$(echo "$todo" | cut -d'|' -f1)
  case "$status" in
    pending) ((count_pending++)) || true ;;
    in-progress|in_progress) ((count_in_progress++)) || true ;;
    done) ((count_done++)) || true ;;
  esac
done

# 출력
case "$OUTPUT_FORMAT" in
  json)
    echo "["
    first=true
    for todo in "${SORTED_TODOS[@]+"${SORTED_TODOS[@]}"}"; do
      [ -z "$todo" ] && continue
      IFS='|' read -r status priority created deadline id title file <<< "$todo"
      [ "$first" = true ] || echo ","
      first=false
      cat << EOF
  {
    "status": "$status",
    "priority": "$priority",
    "created": "$created",
    "deadline": "$deadline",
    "id": "$id",
    "title": "$title",
    "file": "$file"
  }
EOF
    done
    echo "]"
    ;;

  plain)
    for todo in "${SORTED_TODOS[@]+"${SORTED_TODOS[@]}"}"; do
      [ -z "$todo" ] && continue
      echo "$todo" | tr '|' '\t'
    done
    ;;

  human)
    if [ "$SUMMARY" = true ]; then
      echo "[${PROJECT}] TODO: ${count_pending} pending, ${count_in_progress} in-progress"
    else
      echo ""
      echo "[${PROJECT}] TODO 현황"
      print_separator 70

      printf "%-12s %-8s %-12s %s\n" "상태" "우선순위" "생성일" "제목"
      print_separator 70

      for todo in "${SORTED_TODOS[@]+"${SORTED_TODOS[@]}"}"; do
        [ -z "$todo" ] && continue
        IFS='|' read -r status priority created deadline id title file <<< "$todo"

        # 상태 표시
        case "$status" in
          in-progress|in_progress) status_display="▶ in-prog" ;;
          pending)                 status_display="○ pending" ;;
          done)                    status_display="✓ done" ;;
          *)                       status_display="? $status" ;;
        esac

        # 우선순위 표시
        case "$priority" in
          critical) priority_display="CRITICAL" ;;
          urgent)   priority_display="URGENT" ;;
          high)     priority_display="high" ;;
          medium)   priority_display="medium" ;;
          low)      priority_display="low" ;;
          *)        priority_display="$priority" ;;
        esac

        printf "%-12s %-8s %-12s %s\n" "$status_display" "$priority_display" "$created" "$title"
      done

      print_separator 70
      echo "합계: ${count_in_progress} in-progress, ${count_pending} pending"
    fi
    ;;
esac
