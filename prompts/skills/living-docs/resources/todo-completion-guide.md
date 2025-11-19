# TODO 완료 후 Knowledge 통합 가이드

TODO가 `status: done`이 되면 두 가지 옵션이 있습니다:
- **옵션 1**: Knowledge로 통합 (권장)
- **옵션 2**: 파일만 삭제

## 옵션 1: Knowledge로 통합 (권장)

TODO의 내용이 지식으로 남을 가치가 있는 경우, 다음 5단계를 따릅니다:

### Step 1: Category 결정

TODO의 내용을 분석하여 적절한 category 선택:

| TODO 내용 | Category | 파일 위치 예시 |
|-----------|----------|----------------|
| 아키텍처 패턴, 설계 결정 | architecture | `knowledge/architecture/api-design.md` |
| 보안 정책, 주의사항 | security | `knowledge/security/auth-policy.md` |
| 운영 절차, 배포 방법 | operations | `knowledge/operations/deployment.md` |
| 성능 최적화, 튜닝 | performance | `knowledge/performance/caching.md` |
| 기술 스택, 라이브러리 선택 | tech-stack | `knowledge/tech-stack/frontend.md` |
| 팀 프로세스, 개발 규칙 | process | `knowledge/process/code-review.md` |
| 특정 도메인 지식 | domain | `knowledge/domain/payment-flow.md` |
| 문제 해결, 트러블슈팅 | troubleshooting | `knowledge/troubleshooting/db-connection.md` |

### Step 2: 관련 문서 찾기

선택한 category에서 관련 문서 검색:

```bash
# 방법 1: 키워드 검색 (빠름)
Grep "키워드" --type=md ~/docs/{project}/knowledge/{category}/ -i

# 방법 2: 파일명 패턴 검색
Glob ~/docs/{project}/knowledge/{category}/*{keyword}*.md

# 방법 3: 태그 검색 (가장 정확)
Grep "tags:.*키워드" ~/docs/{project}/knowledge/{category}/ -i
```

**검색 우선순위**:
1. 태그가 80%+ 일치하는 문서 → 병합 강력 권장
2. 제목/slug에 키워드 포함 → 통합 후보
3. 검색 결과 없음 → 새 문서 생성

### Step 3: 통합 가치 판단

다음 체크리스트로 통합 여부 결정:

- [ ] 이 지식이 미래의 작업/결정에 참고될 가능성이 있는가?
- [ ] 다른 팀원도 알아야 할 내용인가?
- [ ] 비슷한 상황에서 다시 찾아볼 내용인가?

**모두 No** → 옵션 2 (파일 삭제)로 전환
**하나라도 Yes** → Step 4로 진행

### Step 4A: 기존 문서에 통합 (관련 문서 발견 시)

1. **통합 위치 결정**:
   - **새로운 섹션 추가**: TODO의 내용이 독립적인 주제인 경우
   - **기존 섹션 확장**: 이미 다루고 있는 주제와 관련된 경우
   - **예시/사례 추가**: 구체적인 구현 사례인 경우

2. **내용 추출 및 정리**:
   - TODO의 "## 설명", "## 작업 내용" 섹션에서 핵심 내용 추출
   - 일시적인 정보(날짜, 담당자 등) 제거
   - 재사용 가능한 형태로 재작성

3. **문서 업데이트**:
   ```markdown
   # [기존 문서 제목]

   [기존 내용...]

   ## [새로운 섹션 또는 기존 섹션 확장]

   [TODO에서 추출한 지식]

   ### 관련 사례

   **[[todo-xxx]] 작업 완료 (YYYY-MM-DD)**:
   - [작업에서 배운 점]
   - [구체적인 구현 방법]
   - [주의사항]
   ```

4. **Frontmatter 갱신**:
   ```yaml
   updated: YYYY-MM-DD
   references:
     - "[[todo-xxx]]"  # 추가
   ```

5. **히스토리 추가** (간결성 원칙 준수: 3-5줄):
   ```markdown
   ## 히스토리

   ### YYYY-MM-DD: [[todo-xxx]] 작업 완료 반영
   **변경 내용:** [TODO에서 수행한 작업 내용]
   **추가된 지식:** [새로 추가된 섹션/내용 요약]
   **관련 작업:** [[todo-xxx]]

   ---
   ```

### Step 4B: 새 문서 생성 (관련 문서 없음 시)

1. **파일명 규칙**:
   ```
   ~/docs/{project}/knowledge/{category}/{topic}.md
   ```
   - `{topic}`: 2-4 단어, kebab-case
   - 예: `api-versioning-strategy.md`, `redis-session-store.md`

2. **Frontmatter 작성**:
   ```yaml
   ---
   id: know-{category}-{topic}
   created: YYYY-MM-DD
   updated: YYYY-MM-DD
   tags:
     - {category}
     - [관련 키워드 1]
     - [관련 키워드 2]
   references:
     - "[[todo-xxx]]"
   ---
   ```

3. **초기 구조** (간결성 원칙 준수):
   ```markdown
   # {주제명}

   ## 개요

   [이 지식이 필요한 이유, 배경 설명 - 2-4문장]

   ## 내용

   [TODO에서 추출한 핵심 지식]

   ### [하위 섹션 1]

   [상세 내용]

   ### [하위 섹션 2]

   [상세 내용]

   ## 예시

   [구체적인 코드나 구현 사례]

   ## 주의사항

   [알아야 할 함정, 제약사항]

   ## 관련 문서

   - [[dec-xxx]]: [관련 의사결정]
   - [[know-xxx]]: [관련 지식]

   ## 히스토리

   ### YYYY-MM-DD: [[todo-xxx]] 작업 완료 후 문서화
   **내용:** [초기 작성 이유 - 1-2줄]

   ---
   ```

### Step 5: TODO 파일 삭제

```bash
rm ~/docs/{project}/todos/{slug}.md
```

**완료 메시지**:
```
✅ TODO [[todo-xxx]] 완료 처리
📋 Knowledge 통합: ~/docs/{project}/knowledge/{category}/{topic}.md
   - [통합된 섹션명]
   - [추가된 내용 요약]
```

## 옵션 2: 파일 삭제

단순 작업으로 별도 기록이 필요 없는 경우:

```bash
rm ~/docs/{project}/todos/{slug}.md
```

## 사용자 확인 메시지

"작업이 완료되었습니다. 이 TODO를:"
- [1] Knowledge 문서에 통합 후 삭제 (권장)
- [2] 파일만 삭제
