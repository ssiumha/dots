#!/bin/bash
# lib/utils.sh - 공통 유틸리티 함수

# 색상 정의 (터미널 출력용)
readonly RED='\033[0;31m'
readonly GREEN='\033[0;32m'
readonly YELLOW='\033[0;33m'
readonly BLUE='\033[0;34m'
readonly NC='\033[0m' # No Color

# 프로젝트명 추출 (현재 경로에서)
# ~/repos/{project}/... 또는 ~/repos/{project}_slot1/... 패턴 지원
detect_project() {
  local cwd="${1:-$(pwd)}"
  local project=""

  # ~/repos/{project}/... 패턴에서 추출
  if [[ "$cwd" =~ $HOME/repos/([^/]+) ]]; then
    project="${BASH_REMATCH[1]}"
    # _slot숫자 제거, 끝 숫자 제거
    project=$(echo "$project" | sed 's/_slot[0-9]*$//' | sed 's/[0-9]*$//')
  fi

  echo "$project"
}

# 문서 디렉토리 경로 반환
get_docs_dir() {
  local project="$1"
  echo "$HOME/docs/${project}"
}

# TODO 디렉토리 경로 반환
get_todos_dir() {
  local project="$1"
  echo "$HOME/docs/${project}/todos"
}

# 문서 디렉토리 존재 여부 확인
docs_dir_exists() {
  local project="$1"
  local docs_dir
  docs_dir=$(get_docs_dir "$project")
  [ -d "$docs_dir" ]
}

# 에러 메시지 출력
error() {
  echo -e "${RED}[ERROR]${NC} $*" >&2
}

# 경고 메시지 출력
warn() {
  echo -e "${YELLOW}[WARNING]${NC} $*" >&2
}

# 정보 메시지 출력
info() {
  echo -e "${BLUE}[INFO]${NC} $*"
}

# 성공 메시지 출력
success() {
  echo -e "${GREEN}[OK]${NC} $*"
}

# 테이블 구분선 출력
print_separator() {
  local width="${1:-60}"
  printf '%*s\n' "$width" '' | tr ' ' '━'
}

# 날짜 형식 검증 (YYYY-MM-DD)
is_valid_date() {
  local date="$1"
  [[ "$date" =~ ^[0-9]{4}-[0-9]{2}-[0-9]{2}$ ]]
}

# 오늘 날짜 (YYYY-MM-DD)
today() {
  date +%Y-%m-%d
}

# 이번 달 (YYYY-MM)
this_month() {
  date +%Y-%m
}

# Git uncommitted changes 확인
has_uncommitted_changes() {
  local dir="$1"
  ! git -C "$dir" diff --quiet 2>/dev/null || ! git -C "$dir" diff --cached --quiet 2>/dev/null
}

# 의존성 확인
check_dependency() {
  local cmd="$1"
  if ! command -v "$cmd" &>/dev/null; then
    error "$cmd is required but not installed."
    return 1
  fi
  return 0
}

# 필수 의존성 확인 (yq, rg)
check_required_deps() {
  local missing=()

  for cmd in yq rg; do
    if ! command -v "$cmd" &>/dev/null; then
      missing+=("$cmd")
    fi
  done

  if [ ${#missing[@]} -gt 0 ]; then
    error "Missing required dependencies: ${missing[*]}"
    echo "Install with: brew install ${missing[*]}" >&2
    return 1
  fi

  return 0
}
