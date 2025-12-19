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

# Sum weekly tokens (input + output) from JSONL files
CLAUDE_DIR="$HOME/.claude/projects"
WEEKLY_TOKENS=0
if [ -d "$CLAUDE_DIR" ]; then
  # Sum both input_tokens and output_tokens
  WEEKLY_TOKENS=$(find "$CLAUDE_DIR" -name "*.jsonl" -mtime -"$days_since_tue" 2>/dev/null | \
    xargs grep -oh '"input_tokens":[0-9]*\|"output_tokens":[0-9]*' 2>/dev/null | \
    cut -d: -f2 | \
    awk '{s+=$1} END {print s+0}')
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
