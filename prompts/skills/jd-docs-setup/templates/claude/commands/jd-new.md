# /jd-new

JD 체계에 맞는 새 문서를 생성합니다.

## CLI 권장

컨텍스트 절약을 위해 CLI 사용 권장:

```bash
jd new adr "Database Selection"
jd new incident "Server Outage"
```

CLI 미설치 시 아래 워크플로우로 진행합니다.

---

## 사용법

```
/jd-new {type} "{title}"
```

### 지원 유형

| 유형 | 영역 | 카테고리 | 템플릿 |
|------|------|----------|--------|
| adr | 20-29 | 21-ADR | 01.01-adr.md |
| design | 20-29 | 22-System-Design | 01.03-system-design.md |
| rfc | 20-29 | 25-RFC | 01.04-rfc.md |
| api | 30-39 | 31-REST-API | 01.02-api-rest.md |
| requirement | 50-59 | 51-Requirements | 01.05-requirement.md |
| meeting | 50-59 | 53-Meetings | 01.06-meeting.md |
| retrospective | 50-59 | 54-Retrospectives | 01.07-retrospective.md |
| incident | 60-69 | 63-Incidents | 01.08-incident.md |
| runbook | 60-69 | 64-Runbooks | 01.09-runbook.md |
| troubleshoot | 70-79 | 71-Troubleshooting | 01.10-troubleshooting.md |

## 워크플로우

1. **ID 결정**
   - JDex에서 해당 카테고리의 마지막 ID 확인
   - 다음 ID 할당 (예: 21.03)
   - 빈 카테고리면 `.10`부터 시작 (`.00-.09`는 Standard Zeros 예약)

2. **파일 생성**
   - 경로: `docs/{area}/{category}/{id}-{slug}.md`
   - 카테고리 디렉토리 없으면 자동 생성
   - 템플릿: `docs/00-09-System/01-Templates/` 내 해당 유형 파일
   - 플레이스홀더 치환: `{{CATEGORY}}`, `{{ID}}`, `{{TITLE}}`, `{{DATE}}`

3. **JDex 업데이트**
   - 새 항목 추가

4. **완료 보고**
   ```
   ✅ Created: docs/20-29-Architecture/21-ADR/21.03-caching-strategy.md
   📝 JDex updated: 21.03 Caching Strategy
   ```

## 예시

```
/jd-new adr "Caching Strategy"
→ docs/20-29-Architecture/21-ADR/21.03-caching-strategy.md

/jd-new incident "Database Connection Pool Exhaustion"
→ docs/60-69-Operations/63-Incidents/63.05-database-connection-pool-exhaustion.md
```

## 대화형 모드

제목 없이 호출하면 대화형으로 진행:

```
/jd-new adr
→ "ADR 제목을 입력하세요:"
→ "컨텍스트를 간단히 설명해주세요:"
```

## 충돌 처리

| 상황 | 동작 |
|------|------|
| ID 충돌 (파일 존재) | 다음 가용 ID 자동 할당 |
| 카테고리 디렉토리 없음 | 자동 생성 |
| JDex 파일 없음 | 에러 + `/jd-docs-setup init` 제안 |
| 알 수 없는 문서 유형 | 에러 + 지원 유형 목록 출력 |

## 플레이스홀더

템플릿 → 문서 생성 시 치환:

| 플레이스홀더 | 설명 | 예시 |
|--------------|------|------|
| `{{CATEGORY}}` | 카테고리 번호 | `21` |
| `{{ID}}` | 문서 ID (2자리) | `03` |
| `{{TITLE}}` | 문서 제목 | `Caching Strategy` |
| `{{DATE}}` | 생성일 | `2024-01-15` |
