# jd-index-automation

JDex (문서 인덱스) 동기화를 자동으로 수행합니다.

## 트리거

- `/jd-new` 실행 후
- docs/ 구조 변경 감지 시
- `jd-health-automation`에서 불일치 감지 시

## 워크플로우

1. **파일 스캔**
   - `docs/` 내 `XX.YY-*.md` 패턴 파일 탐색
   - 템플릿 제외 (`01-Templates/`)

2. **JDex 비교**
   - 파일 있고 JDex 없음 → 추가 대상
   - JDex 있고 파일 없음 → 제거 대상
   - ID 충돌 감지

3. **동기화 실행**
   - 누락 항목 JDex에 추가
   - 통계 업데이트 (총 문서 수, 최종 수정일)

4. **결과 보고**

## 호출 방법

스킬 내부 agent이므로 `general-purpose`로 호출:

```python
Task(
  subagent_type="general-purpose",
  prompt="jd-docs-setup 스킬의 jd-index-automation agent로 동작. JDex 동기화 실행.",
  run_in_background=true
)
```

### 병렬 실행

`jd-health-automation`과 함께:

```
docs/ 변경 감지
    ├── jd-health-automation (품질)
    └── jd-index-automation (동기화)
```

## 도구

- Glob: 문서 파일 탐색
- Grep: JDex 항목 검색
- Read: JDex 읽기
- Edit: JDex 업데이트
- Bash: `jd index update` CLI 호출

## 출력 형식

```
🔄 JDex 동기화 완료

추가됨:
+ 21.03 Caching Strategy
+ 31.02 Order API

제거됨:
- 21.02 (파일 없음)

총 문서: 15
```
