# Completed TODO 아카이브 정책

6개월 이상 경과한 completed TODO를 정리하는 상세 절차입니다.

## 실행 시점

- 사용자가 "오래된 TODO 정리", "completed 정리" 요청 시
- 분기/연도 말 정리 시

## 대상

6개월 이상 경과한 completed TODO

## 실행 절차

### 1. 오래된 TODO 검색

```bash
# 6개월 이전 디렉토리 찾기
find ~/docs/{project}/todos/completed -type d -name "20??-??" | \
  while read dir; do
    # 날짜 비교 로직
  done
```

### 2. 사용자에게 옵션 제안

```
📦 6개월 이상 경과한 completed TODO: {count}개

[1] 유지 (프로젝트 히스토리로 보관)
[2] Git 압축 후 삭제 (공간 정리)
[3] 아카이브 디렉토리로 이동 (todos/archived/)
```

### 3. 선택에 따른 처리

#### 옵션 1: 유지

**작업**: 없음

**언제 선택**:
- 활발한 프로젝트
- 회고 자료로 계속 활용
- 팀 성과 추적 필요

#### 옵션 2: Git 압축 후 삭제

**작업**:
```bash
# Git 히스토리 확인
git log --oneline -- todos/completed/YYYY-MM/

# 파일 삭제
rm -rf ~/docs/{project}/todos/completed/YYYY-MM/

# Git 커밋
git add -A
git commit -m "docs(todo): archive completed todos from YYYY-MM"
```

**언제 선택**:
- 완료된 프로젝트
- 공간 절약 필요
- Git 히스토리만으로 충분

**주의**:
- 파일 완전 삭제 (Git 히스토리로만 복구 가능)
- 신중하게 선택

#### 옵션 3: 아카이브 디렉토리로 이동

**작업**:
```bash
# 아카이브 디렉토리 생성
mkdir -p ~/docs/{project}/todos/archived/

# 오래된 completed 이동
mv ~/docs/{project}/todos/completed/YYYY-MM/ \
   ~/docs/{project}/todos/archived/

# Git 커밋
git add -A
git commit -m "docs(todo): archive old completed todos to archived/"
```

**언제 선택**:
- 장기 프로젝트
- 구조적 정리 필요
- 오래된 것과 최근 것 분리

## 권장 정책

| 프로젝트 유형 | 권장 옵션 | 이유 |
|--------------|----------|------|
| 활발한 프로젝트 | 옵션 1 (유지) | 회고 자료로 활용 |
| 장기 프로젝트 | 옵션 3 (아카이브) | 구조적 정리 |
| 완료된 프로젝트 | 옵션 2 (삭제) | 공간 절약 |

## 디렉토리 구조 (옵션 3 선택 시)

```
~/docs/{project}/todos/
├── {active-todo}.md          # pending, in-progress
├── completed/                 # 최근 6개월
│   ├── 2025-01/
│   └── 2025-02/
└── archived/                  # 6개월 이전
    ├── 2024-06/
    ├── 2024-07/
    └── ...
```

## 자동화 스크립트 (선택)

```bash
#!/bin/bash
# archive-old-todos.sh

PROJECT=$1
MONTHS_AGO=6
CUTOFF_DATE=$(date -d "$MONTHS_AGO months ago" +%Y-%m)

find ~/docs/$PROJECT/todos/completed -type d -name "20??-??" | \
  while read dir; do
    month=$(basename "$dir")
    if [[ "$month" < "$CUTOFF_DATE" ]]; then
      echo "Archiving: $dir"
      mkdir -p ~/docs/$PROJECT/todos/archived/
      mv "$dir" ~/docs/$PROJECT/todos/archived/
    fi
  done
```

사용법:
```bash
chmod +x archive-old-todos.sh
./archive-old-todos.sh {project-name}
```
