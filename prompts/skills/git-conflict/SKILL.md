---
name: git-conflict
description: Resolves Git conflicts (rebase, merge, cherry-pick, stash). Use when encountering merge conflicts during any Git operation.
---

# Git Conflict

Git 작업 중 발생한 충돌을 해결합니다.

## 충돌 유형 감지

```bash
git status
```

| 메시지 | 유형 | 해결 명령 |
|--------|------|----------|
| "rebase in progress" | rebase | `git rebase --continue` |
| "merge in progress" | merge | `git merge --continue` |
| "cherry-pick in progress" | cherry-pick | `git cherry-pick --continue` |
| "Unmerged paths" (stash 후) | stash | `git stash drop` (해결 후) |

## 공통 워크플로우

### 1. 충돌 파일 목록

```bash
git diff --name-only --diff-filter=U
```

### 2. 충돌 마커 분석

각 파일을 Read하여 충돌 마커 확인:

```
<<<<<<< HEAD (또는 ours)
(현재 브랜치) ← 기본 우선
=======
(들어오는 변경)
>>>>>>> {commit/branch}
```

### 3. 해결 전략

**자동 해결 가능**:
| 상황 | 해결 |
|------|------|
| import/module 추가 | 양쪽 다 유지 (병합) |
| 같은 위치에 다른 코드 추가 | 양쪽 다 유지 |
| 중복 import | 하나만 유지 |

**사용자 확인 필요**:
| 상황 | 판단 방법 |
|------|----------|
| 같은 위치 다른 수정 | `git log`로 각 변경 의도 확인 |
| 로직 변경 충돌 | 양쪽 로직 병합 또는 최신 선택 |
| 삭제 vs 수정 | HEAD가 리팩토링이면 삭제 |

```
⚠️ 충돌 해결 확인: {파일명}

HEAD:
{HEAD 코드}

Incoming:
{incoming 코드}

[1] HEAD 유지 (권장)
[2] Incoming 유지
[3] 둘 다 병합
[4] 직접 수정
```

### 4. 충돌 마커 제거

Edit으로 충돌 마커 제거: `<<<<<<<`, `=======`, `>>>>>>>`

### 5. Staging

```bash
git add {충돌_해결된_파일들}
```

---

## 유형별 완료

### Rebase

```bash
git rebase --continue
```

추가 충돌 발생 시 반복. 완료 후:

```bash
# force push 필요 (히스토리 변경됨)
git push --force-with-lease
```

### Merge

```bash
git merge --continue
# 또는
git commit  # merge commit 생성
```

완료 후 일반 push.

### Cherry-pick

```bash
git cherry-pick --continue
```

완료 후 일반 push.

### Stash

```bash
# 충돌 해결 후 staging만 하면 됨
git add {파일들}

# stash 항목 제거
git stash drop
```

---

## 중단 옵션

작업을 취소하고 싶을 때:

| 유형 | 중단 명령 |
|------|----------|
| rebase | `git rebase --abort` |
| merge | `git merge --abort` |
| cherry-pick | `git cherry-pick --abort` |
| stash | `git checkout -- {files}` + `git stash drop` |

---

## 완료 확인

```bash
git status
git log --oneline -10
```

```
✅ 충돌 해결 완료

해결 내역:
1. {파일명} - {해결 방법}
2. ...
```

## Examples

### Rebase 충돌 (자동 해결)

User: "rebase 충돌 해결해줘"
→ `git status` → "rebase in progress"
→ 충돌 파일: `src/index.ts` (import 추가)
→ 자동 해결: 양쪽 import 모두 유지
→ `git add` + `git rebase --continue`
→ `git push --force-with-lease`

### Merge 충돌 (사용자 확인)

User: "merge 충돌 해결해줘"
→ `git status` → "merge in progress"
→ 충돌 파일: `src/api.ts` (로직 수정)
→ 사용자 확인: HEAD vs Incoming
→ Edit으로 병합
→ `git add` + `git commit`

### 중단

User: "rebase 취소해줘"
→ `git rebase --abort`
→ 원래 브랜치 상태로 복구

## 주의사항

- `--force` 대신 `--force-with-lease` 사용 (안전)
- 충돌 해결 후 빌드/테스트 확인 권장
- 복잡한 충돌은 사용자에게 확인 후 진행
