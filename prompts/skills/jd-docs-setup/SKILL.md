---
name: jd-docs-setup
description: Sets up Johnny.Decimal documentation system for projects. Use when initializing docs/ structure, creating JD categories, or organizing project documentation.
---

# jd-docs-setup

Johnny.Decimal 체계 기반 프로젝트 문서화 시스템 셋업

## 트리거

- 프로젝트 문서 구조 초기화
- JD 문서 시스템 도입
- "문서 체계화", "JD 문서", "docs 초기화"

## 범위

- `.claude/` 구조 생성 (rules, commands)
- `docs/` 디렉토리 구조 생성 (JD 체계)
- 설정 파일 및 JDex 초기화

## 워크플로우

### 1. 프로젝트 초기화 (init)

```
/jd-docs-setup init
```

#### 단계

1. **현황 분석**
   - 기존 `docs/`, `.claude/` 확인
   - 충돌 가능성 파악

2. **영역 선택** (AskUserQuestion)
   ```
   [1] 00-09-System (필수)
   [2] 10-19-Overview
   [3] 20-29-Architecture (권장)
   [4] 30-39-API (권장)
   [5] 40-49-Development (권장)
   [6] 50-59-Process (권장)
   [7] 60-69-Operations
   [8] 70-79-Knowledge
   [9] 80-89-Reference
   [10] 90-99-Archive
   [R] 권장 세트 (00-09, 20-29, 30-39, 40-49, 50-59)
   ```

3. **정보 수집** (AskUserQuestion)
   - 프로젝트 이름
   - 프로젝트 설명 (optional)
   - 주 언어/프레임워크 (optional)

4. **생성**
   - 선택된 영역별 디렉토리 (예: `docs/20-29-Architecture/`)
   - 필수 디렉토리: `00-Index/`, `01-Templates/`
   - `.claude/rules/jd-docs.md`
   - `.claude/commands/jd-*.md`
   - `jd-docs.yaml` 설정 파일
   - `docs/00-09-System/00-Index/00.00-jdex.md`
   - 플레이스홀더 치환:
     - 프로젝트: `{{PROJECT_NAME}}`, `{{PROJECT_DESCRIPTION}}`, `{{LANGUAGE}}`
     - 날짜: `{{DATE}}`, `{{CURRENT_DATE}}`
     - 문서별: `{{CATEGORY}}`, `{{ID}}`, `{{TITLE}}`, `{{BASE_URL}}`
     - JDex: `{{COUNT}}` (총 문서 수)

   **Note**: 카테고리 디렉토리 (`21-ADR/` 등)는 첫 문서 생성 시 자동 생성

   **충돌 처리**:
   - 기존 디렉토리 존재 → 스킵 (메시지 출력)
   - 기존 파일 존재 → 사용자 확인 또는 스킵

5. **완료 보고**
   ```
   ✅ JD 문서 시스템 초기화 완료

   생성된 영역:
   - 00-09-System
   - 20-29-Architecture
   - ...

   사용 가능한 명령:
   - /jd-new {type} "{title}"
   - /jd-index {action}
   - /jd-health
   ```

### 2. 문서 생성 (/jd-new)

```
/jd-new adr "데이터베이스 선택"
/jd-new api "User API"
/jd-new incident "서버 장애"
```

→ 상세: [jd-new.md](./templates/claude/commands/jd-new.md)

### 3. 인덱스 관리 (/jd-index)

```
/jd-index update    # JDex 동기화
/jd-index check     # 일관성 검사
/jd-index list      # 카테고리별 목록
```

→ 상세: [jd-index.md](./templates/claude/commands/jd-index.md)

### 4. 건강도 체크 (/jd-health)

```
/jd-health          # 전체 점검
/jd-health --fix    # 자동 수정
```

→ 상세: [jd-health.md](./templates/claude/commands/jd-health.md)

## 생성 구조

### docs/ 디렉토리

```
docs/
├── 00-09-System/
│   ├── 00-Index/
│   │   └── 00.00-jdex.md
│   └── 01-Templates/
│       └── (문서 템플릿들)
├── 20-29-Architecture/
│   ├── 21-ADR/
│   ├── 22-System-Design/
│   └── 25-RFC/
├── 30-39-API/
│   └── 31-REST-API/
└── ...
```

### .claude/ 구조

```
.claude/
├── rules/
│   ├── jd-docs.md          # 문서 작성 규칙
│   └── code-docs-link.md   # 코드-문서 연결 (선택)
└── commands/
    ├── jd-new.md
    ├── jd-index.md
    └── jd-health.md
```

## 설계 원칙

1. **선택적 생성**: 필요한 영역만 생성
2. **기존 구조 존중**: 충돌 시 사용자 확인
3. **점진적 확장**: 최소 구조로 시작, 필요시 추가
4. **대화형 설정**: AskUserQuestion으로 의도 확인

## CLI 설치 (선택)

init 완료 후 CLI 도구 설치로 컨텍스트 효율성 향상:

```bash
# jd-docs-setup 스킬 디렉토리에서 실행
cd /path/to/jd-docs-setup  # 또는 스킬이 설치된 위치
cp templates/bin/jd ~/bin/
cp -r templates/bin/jd.d ~/bin/
chmod +x ~/bin/jd ~/bin/jd.d/*
```

### CLI 명령어

```bash
jd new adr "Title"    # 문서 생성
jd index update       # JDex 동기화
jd health             # 건강도 체크
jd health --fix       # 자동 수정
```

### CLI 확장

새 서브커맨드 추가:

```bash
# ~/bin/jd.d/stats 생성 → jd stats로 호출 가능
```

사용자가 "jd에 stats 커맨드 추가해줘"라고 요청하면:
1. `~/bin/jd.d/stats` 스크립트 생성
2. `jd stats` 명령으로 즉시 사용 가능

## 코드-문서 연결

코드와 문서의 밀접한 연결을 위해 `code-docs-link.md` rule 제공:

- `@doc 21.01` 주석 컨벤션
- 폴더-카테고리 매핑 가이드
- 자동 문서 로드 설정

→ 상세: [code-docs-link.md](./templates/claude/rules/code-docs-link.md)

## Agent 자동화

반복 작업을 subagent로 자동화:

### 사용 가능한 Agent

| Agent | 트리거 | 용도 |
|-------|--------|------|
| jd-health-automation | docs/ 2+ 파일 수정 후 | 건강도 검사, 자동 수정 제안 |
| jd-index-automation | /jd-new 후, 구조 변경 시 | JDex 동기화 |
| jd-docstring-linker | src/ 파일 수정 후 | @doc 참조 검증 |

### 병렬 실행 패턴

스킬 내부 agent는 `general-purpose`로 호출:

**문서 변경 후 검증:**
```python
Task(subagent_type="general-purpose", prompt="jd-health-automation: 건강도 검사", run_in_background=true)
Task(subagent_type="general-purpose", prompt="jd-index-automation: JDex 동기화", run_in_background=true)
```

**종합 검사 (3개 병렬):**
```python
Task(subagent_type="general-purpose", prompt="jd-health-automation: docs/ 건강도", run_in_background=true)
Task(subagent_type="general-purpose", prompt="jd-index-automation: JDex 동기화", run_in_background=true)
Task(subagent_type="general-purpose", prompt="jd-docstring-linker: @doc 검사", run_in_background=true)
```

→ 상세: [agents/](./agents/)

## 참조

- [REFERENCE.md](./REFERENCE.md) - JD 네이밍 규칙
- [resources/01-jd-area-catalog.md](./resources/01-jd-area-catalog.md) - 영역별 상세

## 문서 유형 → 카테고리 매핑

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
