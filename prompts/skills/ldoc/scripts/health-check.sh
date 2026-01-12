#!/bin/bash
# health-check.sh - 문서 건강도 체크

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

# shellcheck source=lib/utils.sh
source "${SCRIPT_DIR}/lib/utils.sh"
# shellcheck source=lib/frontmatter.sh
source "${SCRIPT_DIR}/lib/frontmatter.sh"

usage() {
  cat << EOF
Usage: $(basename "$0") [options]

문서 건강도를 체크합니다.

Options:
  -p, --project PROJECT   프로젝트명 (기본: 현재 디렉토리에서 추론)
  --quick                 빠른 체크만 (파일 크기)
  --standard              표준 체크 (크기 + 링크) (기본)
  --full                  전체 체크 (크기 + 링크 + 중복)
  --json                  JSON 형식으로 출력
  -h, --help              도움말

Examples:
  $(basename "$0")                # 표준 체크
  $(basename "$0") --quick        # 빠른 체크
  $(basename "$0") --full         # 전체 체크
EOF
}

# 기본값
PROJECT=""
CHECK_LEVEL="standard"  # quick, standard, full
OUTPUT_FORMAT="human"

# 옵션 파싱
while [[ $# -gt 0 ]]; do
  case $1 in
    -p|--project)
      PROJECT="$2"
      shift 2
      ;;
    --quick)
      CHECK_LEVEL="quick"
      shift
      ;;
    --standard)
      CHECK_LEVEL="standard"
      shift
      ;;
    --full)
      CHECK_LEVEL="full"
      shift
      ;;
    --json)
      OUTPUT_FORMAT="json"
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

# 문서 디렉토리 확인 (상위 탐색)
DOCS_DIR=$(get_docs_dir)
if [ -z "$DOCS_DIR" ]; then
  error "Cannot find docs/ directory. Run from within a project."
  exit 1
fi

PROJECT=$(detect_project)

if [ ! -d "$DOCS_DIR" ]; then
  error "Docs directory not found: $DOCS_DIR"
  exit 1
fi

# 결과 저장
declare -a ISSUES_CRITICAL=()
declare -a ISSUES_WARNING=()
declare -a ISSUES_INFO=()

# 체크 1: 파일 크기
check_file_size() {
  while IFS= read -r file; do
    [ -f "$file" ] || continue

    lines=$(wc -l < "$file")
    relpath="${file#$DOCS_DIR/}"

    if [ "$lines" -ge 500 ]; then
      ISSUES_CRITICAL+=("[SIZE] $relpath: ${lines} lines (500+ critical)")
    elif [ "$lines" -ge 300 ]; then
      ISSUES_WARNING+=("[SIZE] $relpath: ${lines} lines (300+ warning)")
    fi
  done < <(find "$DOCS_DIR" -name "*.md" -type f 2>/dev/null)
}

# 체크 2: 끊어진 링크
check_broken_links() {
  # [[id]] 패턴 추출
  while IFS= read -r match; do
    file=$(echo "$match" | cut -d: -f1)
    link=$(echo "$match" | grep -oE '\[\[[^\]]+\]\]' | head -1)
    id=$(echo "$link" | tr -d '[]')

    # ID로 파일 찾기 (know-, dec-, todo- 프리픽스)
    local found=false

    # 직접 ID 검색
    if rg -l "^id: ${id}$" "$DOCS_DIR" &>/dev/null; then
      found=true
    fi

    # 파일명에서 찾기
    if [ "$found" = false ]; then
      local search_name="${id#know-}"
      search_name="${search_name#dec-}"
      search_name="${search_name#todo-}"
      if find "$DOCS_DIR" -name "*${search_name}*.md" -type f 2>/dev/null | grep -q .; then
        found=true
      fi
    fi

    if [ "$found" = false ]; then
      relpath="${file#$DOCS_DIR/}"
      ISSUES_WARNING+=("[LINK] $relpath: broken link $link")
    fi
  done < <(rg '\[\[[^\]]+\]\]' "$DOCS_DIR" --type md 2>/dev/null || true)
}

# 체크 3: 태그 중복 (간단 버전)
check_tag_duplicates() {
  # 태그별 파일 수 계산
  declare -A tag_files

  while IFS= read -r file; do
    [ -f "$file" ] || continue

    tags=$(get_tags "$file")
    [ -z "$tags" ] && continue

    IFS=',' read -ra tag_array <<< "$tags"
    for tag in "${tag_array[@]}"; do
      tag=$(echo "$tag" | xargs)  # trim
      if [ -n "$tag" ]; then
        tag_files["$tag"]="${tag_files[$tag]:-0}"
        ((tag_files["$tag"]++)) || true
      fi
    done
  done < <(find "$DOCS_DIR" -name "*.md" -type f 2>/dev/null)

  # 많이 사용된 태그 보고
  for tag in "${!tag_files[@]}"; do
    count="${tag_files[$tag]}"
    if [ "$count" -ge 10 ]; then
      ISSUES_INFO+=("[TAG] '$tag' used in ${count} files (consider subcategory)")
    fi
  done
}

# 체크 실행
check_file_size

if [ "$CHECK_LEVEL" = "standard" ] || [ "$CHECK_LEVEL" = "full" ]; then
  check_broken_links
fi

if [ "$CHECK_LEVEL" = "full" ]; then
  check_tag_duplicates
fi

# 통계 계산
total_files=$(find "$DOCS_DIR" -name "*.md" -type f 2>/dev/null | wc -l | xargs)
total_issues=$((${#ISSUES_CRITICAL[@]} + ${#ISSUES_WARNING[@]}))

# 건강도 점수 계산 (100점 만점)
score=100
score=$((score - ${#ISSUES_CRITICAL[@]} * 15))
score=$((score - ${#ISSUES_WARNING[@]} * 5))
[ $score -lt 0 ] && score=0

# 출력
case "$OUTPUT_FORMAT" in
  json)
    cat << EOF
{
  "project": "$PROJECT",
  "check_level": "$CHECK_LEVEL",
  "total_files": $total_files,
  "score": $score,
  "issues": {
    "critical": ${#ISSUES_CRITICAL[@]},
    "warning": ${#ISSUES_WARNING[@]},
    "info": ${#ISSUES_INFO[@]}
  },
  "details": {
    "critical": $(printf '%s\n' "${ISSUES_CRITICAL[@]:-}" | jq -R -s 'split("\n") | map(select(length > 0))'),
    "warning": $(printf '%s\n' "${ISSUES_WARNING[@]:-}" | jq -R -s 'split("\n") | map(select(length > 0))'),
    "info": $(printf '%s\n' "${ISSUES_INFO[@]:-}" | jq -R -s 'split("\n") | map(select(length > 0))')
  }
}
EOF
    ;;

  human)
    echo ""
    echo "[${PROJECT}] Documentation Health Report"
    print_separator 60
    echo ""
    echo "Check Level: $CHECK_LEVEL"
    echo "Total Files: $total_files"
    echo ""

    if [ ${#ISSUES_CRITICAL[@]} -gt 0 ]; then
      echo -e "${RED}Critical Issues (${#ISSUES_CRITICAL[@]}):${NC}"
      for issue in "${ISSUES_CRITICAL[@]}"; do
        echo "  $issue"
      done
      echo ""
    fi

    if [ ${#ISSUES_WARNING[@]} -gt 0 ]; then
      echo -e "${YELLOW}Warnings (${#ISSUES_WARNING[@]}):${NC}"
      for issue in "${ISSUES_WARNING[@]}"; do
        echo "  $issue"
      done
      echo ""
    fi

    if [ ${#ISSUES_INFO[@]} -gt 0 ]; then
      echo -e "${BLUE}Info (${#ISSUES_INFO[@]}):${NC}"
      for issue in "${ISSUES_INFO[@]}"; do
        echo "  $issue"
      done
      echo ""
    fi

    if [ $total_issues -eq 0 ]; then
      success "No issues found!"
      echo ""
    fi

    print_separator 60

    # 점수 표시
    if [ $score -ge 80 ]; then
      echo -e "Health Score: ${GREEN}${score}/100${NC} (Good)"
    elif [ $score -ge 60 ]; then
      echo -e "Health Score: ${YELLOW}${score}/100${NC} (Fair)"
    else
      echo -e "Health Score: ${RED}${score}/100${NC} (Needs Attention)"
    fi
    ;;
esac
