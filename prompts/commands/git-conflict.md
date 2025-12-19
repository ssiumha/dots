---
description: Git rebase 충돌 해결 (conflict 발생 시 사용)
---

# Git Conflict

rebase 중 발생한 충돌을 해결합니다.

## Instructions

### 1. 현재 상태 확인

```bash
git status
```

- "rebase in progress" 확인
- 현재 진행 중인 커밋 확인

### 2. 충돌 파일 목록 확인

```bash
git diff --name-only --diff-filter=U
```

### 3. 각 충돌 파일 분석

각 파일을 Read하여 충돌 마커 확인:

```
<<<<<<< HEAD
(현재 브랜치 - main/target) ← 기본 우선
=======
(들어오는 변경 - feature)
>>>>>>> {commit}
```

### 4. 충돌 해결 전략

**기본 원칙**: HEAD (main/target) 우선

**자동 해결 가능**:
- import/module 추가 → 양쪽 다 유지 (병합)
- 같은 위치에 다른 코드 추가 → 양쪽 다 유지
- 중복 import → 하나만 유지

**사용자 확인 필요**:
- 같은 코드를 다르게 수정한 경우
- 로직 변경이 충돌하는 경우
- 삭제 vs 수정 충돌

```
⚠️ 충돌 해결 확인 필요: {파일명}

HEAD (main):
{HEAD 코드}

Feature:
{feature 코드}

[1] HEAD 유지 (권장)
[2] Feature 유지
[3] 둘 다 병합
[4] 직접 수정
```

### 5. 충돌 마커 제거

Edit 도구로 충돌 마커 제거하고 올바른 코드 유지.

**주의**: 충돌 마커 (`<<<<<<<`, `=======`, `>>>>>>>`) 완전히 제거

### 6. Staging 및 Continue

```bash
git add {충돌_해결된_파일들} && git rebase --continue
```

### 7. 반복

추가 충돌 발생 시 3-6 반복.

### 8. 완료 확인

```bash
git status
git log --oneline -10
```

rebase 완료 후:

```
✅ Rebase 완료

충돌 해결 내역:
1. {파일명} - {해결 방법 요약}
2. ...

Push 하시겠습니까? (rebase 후라 force push 필요)
[1] git push --force-with-lease
[2] 나중에
```

## 주의사항

- `--force` 대신 `--force-with-lease` 사용 (안전)
- 충돌 해결 후 빌드/테스트 확인 권장
- 복잡한 충돌은 사용자에게 확인 후 진행
