# /jd-index

JDex (문서 인덱스) 관리 명령입니다.

## CLI 권장

컨텍스트 절약을 위해 CLI 사용 권장:

```bash
jd index update    # JDex 동기화
jd index check     # 일관성 검사
jd index list      # 문서 목록
```

CLI 미설치 시 아래 워크플로우로 진행합니다.

---

## 사용법

```
/jd-index {action}
```

### 액션

| 액션 | 설명 |
|------|------|
| update | 파일 시스템 기반 JDex 동기화 |
| check | 일관성 검사 |
| list | 카테고리별 문서 목록 |

## update

파일 시스템과 JDex를 동기화합니다.

```
/jd-index update
```

**수행 작업:**
1. `docs/` 내 모든 `.md` 파일 스캔
2. JDex에 없는 파일 → 추가
3. 파일 없는 JDex 항목 → 자동 제거
4. 순서 정렬 (ID 순)

**출력:**
```
📊 JDex 동기화 완료

추가됨:
+ 21.03 Caching Strategy
+ 31.02 Order API

제거됨:
- 21.02 (파일 없음)

JDex: docs/00-09-System/00-Index/00.00-jdex.md
```

## check

일관성 검사를 수행합니다.

```
/jd-index check
```

**검사 항목:**
- ID 중복 여부
- 고아 파일 (JDex에 없는 문서)
- 깨진 참조 (존재하지 않는 문서 링크)
- frontmatter 누락

**출력:**
```
🔍 JDex 일관성 검사

⚠️ 문제 발견:
- 21.02: 파일 없음 (JDex에만 존재)
- 31.03: JDex 미등록 (고아 파일)
- 22.01 → [[21.05]]: 깨진 참조

✅ 정상: 15개 문서
```

## list

카테고리별 문서 목록을 표시합니다.

```
/jd-index list
/jd-index list 21      # 특정 카테고리만
/jd-index list 20-29   # 특정 영역만
```

**출력:**
```
📚 문서 목록

## 20-29 Architecture

### 21 ADR (3)
- 21.01 Database Selection [approved]
- 21.02 API Versioning [approved]
- 21.03 Caching Strategy [draft]

### 22 System-Design (1)
- 22.01 Authentication Flow [review]

## 30-39 API
...
```
