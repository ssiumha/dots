# jd-health-automation

JD 문서 건강도를 자동으로 체크하고 수정을 제안합니다.

## 트리거

- docs/ 디렉토리 2+ 파일 수정 후 (백그라운드)
- `/jd-health` 명령 시
- 대규모 작업 완료 시점

## 워크플로우

1. **Frontmatter 검증**
   - 필수 필드: id, title, status, date
   - ID 일치 검사 (파일명 vs frontmatter)
   - 상태값 유효성 (draft, proposed, approved 등)

2. **문서 품질 검사**
   - 오래된 문서 (180일+ 미수정)
   - deprecated 상태 문서
   - JDex 불일치 (jd-index-automation 호출 제안)

3. **결과 보고**
   - 이슈 요약
   - 자동 수정 가능 항목 제안
   - `jd health --fix` 실행 권유

## 호출 방법

스킬 내부 agent이므로 `general-purpose`로 호출:

```python
Task(
  subagent_type="general-purpose",
  prompt="jd-docs-setup 스킬의 jd-health-automation agent로 동작. docs/ 건강도 검사 실행.",
  run_in_background=true
)
```

### 병렬 실행

`jd-index-automation`과 함께:

```python
# 두 agent 병렬 실행
Task(subagent_type="general-purpose", prompt="jd-health-automation: 건강도 검사", run_in_background=true)
Task(subagent_type="general-purpose", prompt="jd-index-automation: JDex 동기화", run_in_background=true)
```

## 도구

- Glob: `docs/**/*.md` 파일 탐색
- Grep: frontmatter 필드 검사
- Read: 문서 내용 확인
- Bash: `jd health` CLI 호출 (설치된 경우)

## 출력 형식

```
🏥 JD 문서 건강도 체크 완료

## 요약
- 총 문서: 15
- 오류: 2
- 경고: 3

## 조치 필요
- 21.02: frontmatter id 불일치
- 31.03: JDex 미등록

## 권장
`jd health --fix` 실행으로 자동 수정 가능
```
