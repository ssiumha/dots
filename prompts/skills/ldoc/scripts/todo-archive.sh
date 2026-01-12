#!/bin/bash
# todo-archive.sh - 완료된 TODO 아카이브

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

# shellcheck source=lib/utils.sh
source "${SCRIPT_DIR}/lib/utils.sh"
# shellcheck source=lib/frontmatter.sh
source "${SCRIPT_DIR}/lib/frontmatter.sh"

usage() {
  cat << EOF
Usage: $(basename "$0") [options]

완료된 TODO(status: done)를 월별 폴더로 아카이브합니다.

Options:
  -p, --project PROJECT   프로젝트명 (기본: 현재 디렉토리에서 추론)
  -n, --dry-run           실제 이동 없이 미리보기 (기본)
  --execute               실제로 파일 이동 실행
  --json                  JSON 형식으로 출력
  -h, --help              도움말

Examples:
  $(basename "$0")                # dry-run으로 미리보기
  $(basename "$0") --execute      # 실제 아카이브 실행
EOF
}

# 기본값
PROJECT=""
DRY_RUN=true
OUTPUT_FORMAT="human"

# 옵션 파싱
while [[ $# -gt 0 ]]; do
  case $1 in
    -p|--project)
      PROJECT="$2"
      shift 2
      ;;
    -n|--dry-run)
      DRY_RUN=true
      shift
      ;;
    --execute)
      DRY_RUN=false
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

TODOS_DIR=$(get_todos_dir)
PROJECT=$(detect_project)

if [ ! -d "$TODOS_DIR" ]; then
  error "TODO directory not found: $TODOS_DIR"
  exit 1
fi

# Git 상태 확인 (execute 모드일 때만)
if [ "$DRY_RUN" = false ] && [ -d "${DOCS_DIR}/.git" ]; then
  if has_uncommitted_changes "$DOCS_DIR"; then
    warn "Uncommitted changes exist in $DOCS_DIR"
    warn "Commit your changes first, or use --dry-run to preview."
  fi
fi

# 완료된 TODO 찾기
declare -a COMPLETED_TODOS=()

while IFS= read -r file; do
  [ -f "$file" ] || continue

  status=$(get_todo_status "$file" || echo "")
  [ -z "$status" ] && continue

  if [ "$status" = "done" ]; then
    COMPLETED_TODOS+=("$file")
  fi
done < <(find "$TODOS_DIR" -maxdepth 1 -name "*.md" -type f 2>/dev/null)

# 완료된 TODO가 없으면 종료
if [ ${#COMPLETED_TODOS[@]} -eq 0 ]; then
  if [ "$OUTPUT_FORMAT" = "json" ]; then
    echo '{"archived": [], "count": 0}'
  else
    info "No completed TODOs to archive."
  fi
  exit 0
fi

# 아카이브 대상 분석
declare -a ARCHIVE_PLAN=()

for file in "${COMPLETED_TODOS[@]}"; do
  filename=$(basename "$file")

  # completed 날짜 확인
  completed_date=$(get_todo_completed "$file")
  if [ -z "$completed_date" ]; then
    completed_date=$(today)
  fi

  # 월별 디렉토리 결정
  year_month="${completed_date:0:7}"  # YYYY-MM
  target_dir="${TODOS_DIR}/completed/${year_month}"
  target_file="${target_dir}/${filename}"

  ARCHIVE_PLAN+=("${file}|${target_dir}|${target_file}|${year_month}")
done

# 출력
case "$OUTPUT_FORMAT" in
  json)
    echo "{"
    echo '  "dry_run": '"$DRY_RUN"','
    echo '  "count": '"${#ARCHIVE_PLAN[@]}"','
    echo '  "archived": ['
    first=true
    for plan in "${ARCHIVE_PLAN[@]}"; do
      IFS='|' read -r src_file target_dir target_file year_month <<< "$plan"
      [ "$first" = true ] || echo ","
      first=false
      cat << EOF
    {
      "source": "$src_file",
      "target": "$target_file",
      "month": "$year_month"
    }
EOF
    done
    echo "  ]"
    echo "}"
    ;;

  human)
    echo ""
    if [ "$DRY_RUN" = true ]; then
      echo "[${PROJECT}] Archive Preview (dry-run)"
    else
      echo "[${PROJECT}] Archiving Completed TODOs"
    fi
    print_separator 60

    echo ""
    echo "Found ${#ARCHIVE_PLAN[@]} completed TODO(s):"
    echo ""

    for plan in "${ARCHIVE_PLAN[@]}"; do
      IFS='|' read -r src_file target_dir target_file year_month <<< "$plan"
      filename=$(basename "$src_file")

      if [ "$DRY_RUN" = true ]; then
        echo "  [DRY-RUN] $filename → completed/${year_month}/"
      else
        # 디렉토리 생성
        mkdir -p "$target_dir"

        # 파일 이동
        if [ -f "$target_file" ]; then
          warn "Target exists, skipping: $target_file"
        else
          mv "$src_file" "$target_file"
          success "$filename → completed/${year_month}/"
        fi
      fi
    done

    echo ""
    print_separator 60

    if [ "$DRY_RUN" = true ]; then
      echo ""
      info "This is a dry-run. Use --execute to actually move files."
    else
      echo ""
      success "Archived ${#ARCHIVE_PLAN[@]} TODO(s)."
      echo ""
      info "Git commit suggestion:"
      echo "  cd ${DOCS_DIR} && git add todos/ && git commit -m \"docs(todo): archive completed TODOs\""
    fi
    ;;
esac
