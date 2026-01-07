---
paths:
  - docs/**
---

# JD 문서 작성 규칙

`docs/` 디렉토리 내 문서 작성 시 Johnny.Decimal 체계를 따릅니다.

## 네이밍 규칙

### 디렉토리

- 영역: `{start}-{end}-{Name}` (예: `20-29-Architecture`)
- 카테고리: `{number}-{Name}` (예: `21-ADR`)

### 파일

```
{category}.{id}-{slug}.md
```

예시: `21.01-database-selection.md`

## ID 할당

1. JDex (`00.00-jdex.md`)에서 해당 카테고리의 마지막 ID 확인
2. 마지막 ID + 1 할당 (빈 카테고리면 `.10`부터 - Standard Zeros 예약)
3. JDex에 새 항목 추가
4. 파일 생성

**Note**: ID `.00-.09`는 Standard Zeros로 예약됨

## 필수 Frontmatter

```yaml
---
id: "21.01"
title: "문서 제목"
status: draft
date: YYYY-MM-DD
---
```

## 상태 전이

```
draft → review → approved → active → deprecated → archived
```

## 문서 간 참조

JD ID로 참조:
- 실제 문서: `[[21.01]]` 또는 `[[21.01 Database Selection]]`
- 템플릿 예시: `[[21.XX]]` (XX는 실제 ID로 대체)

## 체크리스트

문서 작성 시:
- [ ] 올바른 카테고리에 생성
- [ ] ID 중복 없음
- [ ] JDex에 등록됨
- [ ] 필수 frontmatter 포함
- [ ] 관련 문서 링크 연결
