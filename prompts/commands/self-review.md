---
description: 리뷰와 개선을 3회 반복 (self-review cycle)
allowed-tools: Read, Glob, Grep, Edit, Task(code-reviewer)
---

최근 변경된 코드에 대해 리뷰와 개선을 3회 반복합니다.

## 대상 파일

!`if git rev-parse --git-dir > /dev/null 2>&1; then git diff --name-only HEAD; else for dir in */; do [ -d "$dir/.git" ] && git -C "$dir" diff --name-only HEAD 2>/dev/null | while IFS= read -r f; do printf '%s%s\n' "$dir" "$f"; done; done; fi`

## 워크플로우

각 라운드(1/3, 2/3, 3/3)에서:
1. code-reviewer agent로 리뷰
2. 발견된 이슈 수정
3. Issues가 없으면 조기 종료

Round 1/3부터 시작하세요.
