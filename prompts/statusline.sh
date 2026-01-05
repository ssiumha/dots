#!/bin/bash
input=$(cat)

# Basic info
MODEL=$(echo "$input" | jq -r '.model.display_name')
BRANCH=$(git branch --show-current 2>/dev/null)

# Session tokens
TOTAL_INPUT=$(echo "$input" | jq -r '.context_window.total_input_tokens // 0')
TOTAL_OUTPUT=$(echo "$input" | jq -r '.context_window.total_output_tokens // 0')
SESSION_TOKENS=$((TOTAL_INPUT + TOTAL_OUTPUT))

# Format session tokens (K)
if [ "$SESSION_TOKENS" -ge 1000 ]; then
  SESSION_FMT="$((SESSION_TOKENS / 1000))K"
else
  SESSION_FMT="$SESSION_TOKENS"
fi

# Lines changed
LINES_ADDED=$(echo "$input" | jq -r '.cost.total_lines_added // 0')
LINES_REMOVED=$(echo "$input" | jq -r '.cost.total_lines_removed // 0')
LINES_FMT="+$LINES_ADDED -$LINES_REMOVED"

# Weekly usage (from last Tuesday 10am KST)
day_of_week=$(date +%u)  # 1=Mon ~ 7=Sun
hour=$(date +%H)
if [ "$day_of_week" -eq 2 ] && [ "$hour" -ge 10 ]; then
  days_since_tue=0
elif [ "$day_of_week" -gt 2 ]; then
  days_since_tue=$((day_of_week - 2))
elif [ "$day_of_week" -lt 2 ]; then
  days_since_tue=$((day_of_week + 5))
else
  days_since_tue=7
fi

# Calculate exact reset timestamp (last Tuesday 10am)
RESET_DATE=$(date -v-"${days_since_tue}"d +%Y-%m-%d)
RESET_TS="${RESET_DATE} 10:00:00"

# Sum weekly tokens from JSONL files (birthtime-based filtering)
# Rate limit uses input + output only (cache tokens not counted in rate limit)
#
# Calibration history:
# - 2025-12-22: 3.4M (in+out) = 53% → limit ~6.4M
# - 2025-12-23: Fixed mtime→birthtime. in=28K, out=15K = 43K → /usage 1% → limit ~4.3M
#   cache_create=497K, cache_read=3.3M (not counted in rate limit)
CLAUDE_DIR="$HOME/.claude/projects"
WEEKLY_TOKENS=0
if [ -d "$CLAUDE_DIR" ]; then
  # Get reset timestamp as epoch
  RESET_EPOCH=$(date -j -f "%Y-%m-%d %H:%M:%S" "$RESET_TS" +%s 2>/dev/null)

  # Sum input + output from files created after reset (birthtime)
  WEEKLY_TOKENS=$(
    for f in "$CLAUDE_DIR"/*/*.jsonl; do
      BIRTH=$(stat -f "%B" "$f" 2>/dev/null)
      if [ "${BIRTH:-0}" -ge "${RESET_EPOCH:-0}" ] 2>/dev/null; then
        cat "$f"
      fi
    done 2>/dev/null | \
    grep -oh '"input_tokens":[0-9]*\|"output_tokens":[0-9]*' | \
    cut -d: -f2 | \
    awk '{s+=$1} END {print s+0}'
  )
fi

# Weekly limit (env var or default ~7M for Max PRO)
WEEKLY_LIMIT=${CLAUDE_WEEKLY_LIMIT:-7000000}
if [ "$WEEKLY_TOKENS" -gt 0 ] && [ "$WEEKLY_LIMIT" -gt 0 ]; then
  WEEKLY_PCT=$((WEEKLY_TOKENS * 100 / WEEKLY_LIMIT))
else
  WEEKLY_PCT=0
fi

# Format weekly tokens
if [ "$WEEKLY_TOKENS" -ge 1000000 ]; then
  WEEKLY_FMT="$((WEEKLY_TOKENS / 1000000))M"
elif [ "$WEEKLY_TOKENS" -ge 1000 ]; then
  WEEKLY_FMT="$((WEEKLY_TOKENS / 1000))K"
else
  WEEKLY_FMT="$WEEKLY_TOKENS"
fi

echo "[$MODEL] 🌿 $BRANCH │ $SESSION_FMT │ $LINES_FMT │ 📅 $WEEKLY_FMT ($WEEKLY_PCT%)"
