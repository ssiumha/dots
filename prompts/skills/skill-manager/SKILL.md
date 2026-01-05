---
name: skill-manager
description: Skill 생성/수정/갱신을 지원합니다. prompts/skills/ 작업, "skill 만들어줘", "skill 수정", "skill 확장", "skill에 반영" 요청 시 proactively 사용하세요.
---

# Skill Manager

Skill을 생성, 수정, 갱신하고 best practices를 검증합니다.

## 자동 트리거

이 skill은 다음 조건에서 **proactively** 작동합니다:

| 조건 | 예시 |
|------|------|
| "skill" + 작업 동사 | "skill 만들어줘", "skill 수정해줘" |
| skill 이름 언급 + 작업 | "ldoc 확장해줘" |
| prompts/skills/ 파일 작업 | SKILL.md 수정 요청 |

**핵심 철학**:
- 작업 컨텍스트 자동 분석 (대화, 파일 변경)
- Best practices 자동 적용 (기존 skills 패턴)
- 토큰 효율 최적화 (리소스 분리, 템플릿 위임)
- 검증 자동화 (필수 섹션, 분량, 파일명)

## Instructions

### 워크플로우 1: 작업 컨텍스트 분석

사용자가 "여태까지 작업을 skill에 반영해줘" 또는 "skill로 만들어줘" 요청 시:

1. **대화 히스토리 분석**
   - 최근 작업 내용 파악
   - 반복된 패턴 식별
   - 사용된 도구/명령어 추출

2. **파일 변경 확인**
   ```bash
   git status
   git diff --cached
   ```
   - 새로 생성된 파일
   - 수정된 파일
   - 파일 변경 패턴

3. **패턴 추출**
   - 워크플로우 (단계별 작업)
   - 반복 작업 (동일 명령 여러 번)
   - 도구 사용 (특정 CLI, 스크립트)
   - 파일 생성 (템플릿 기반)

4. **워크플로우 2로 진행**

### 워크플로우 2: 기존 Skill 연관성 확인

1. **skills 디렉토리 검색**
   ```bash
   Glob ~/dots/prompts/skills/*/SKILL.md
   ```

2. **키워드 매칭**
   - 작업 내용에서 핵심 키워드 추출
   - 각 skill의 description과 비교
   - Grep으로 skill 내용 검색

3. **연관성 판단**
   - **높은 연관성** (80%+ 일치):
     - 같은 도메인 (예: 문서화, 배포, 테스트)
     - 같은 도구 사용
     - 같은 워크플로우 패턴
     → **워크플로우 3A (갱신)로 진행**

   - **낮은 연관성** (20% 미만):
     - 새로운 도메인
     - 다른 도구/패턴
     → **워크플로우 3B (신규)로 진행**

   - **중간 연관성** (20-80%):
     - 사용자에게 질문: "기존 {skill-name}에 추가 vs 새 skill 생성?"
     - 사용자 선택에 따라 3A 또는 3B

4. **사용자 확인**
   - 발견된 관련 skill 목록 제시
   - 갱신 vs 신규 최종 확인

### 워크플로우 3A: 기존 Skill 갱신

기존 skill에 새로운 내용을 추가할 때:

1. **갱신 유형 결정**

   AskUserQuestion으로 확인:
   - [1] 워크플로우 추가 (새로운 사용 사례)
   - [2] 리소스 추가 (상세 지식 확장)
   - [3] 예시 추가 (새로운 시나리오)
   - [4] 원칙 추가/수정 (best practice)
   - [5] 안티패턴 추가 (주의사항)

2. **갱신 실행**

   **옵션 1: 워크플로우 추가**
   - 기존 SKILL.md Read
   - 마지막 워크플로우 번호 확인
   - 새 워크플로우 추가 (번호 증가)
   - 간결성 유지 (15-30줄)

   **옵션 2: 리소스 추가**
   - resources/ 디렉토리 확인
   - 새 리소스 파일 생성 (번호 매김)
   - SKILL.md에서 참조 추가

   **옵션 3: 예시 추가**
   - Examples 섹션에 추가
   - 플로우 차트식 (간결)

   **옵션 4: 원칙 추가/수정**
   - 중요 원칙 섹션 수정
   - 기존 원칙 개선

   **옵션 5: 안티패턴 추가**
   - 문서 작성 안티패턴 섹션 추가/수정
   - ❌/✅ 형식

3. **검증**
   - 분량 확인 (기존 + 신규 < 600줄)
   - 일관성 확인 (기존 구조 유지)
   - Best practices 체크리스트

4. **Git 커밋**
   ```bash
   cd ~/dots/prompts/skills/{skill-name}
   git add .
   git commit -m "[skill] update {skill-name} - {변경 요약}"
   ```

### 워크플로우 3B: 신규 Skill 생성

새로운 skill을 만들 때:

1. **Skill 이름 결정**
   - kebab-case
   - 2-3 단어 권장
   - 도메인 명확 (예: doc-optimization, deployment-automation)

2. **Skill 유형 선택**

   AskUserQuestion으로 확인:
   - [1] 워크플로우 기반 (단계별 작업, 200-450줄)
   - [2] 리소스 로딩 기반 (키워드 매칭, 140-160줄)
   - [3] Phase 기반 (선형 진행, 200-250줄)
   - [4] 가이드/리뷰 기반 (체크리스트, 280-520줄)
   - [5] 도구 실행 기반 (CLI 래퍼, 90-100줄)

   **상세 템플릿**: `resources/01-type-templates.md` 참조

3. **디렉토리 생성**
   ```bash
   mkdir -p ~/dots/prompts/skills/{skill-name}
   mkdir -p ~/dots/prompts/skills/{skill-name}/resources
   mkdir -p ~/dots/prompts/skills/{skill-name}/templates
   ```

4. **SKILL.md 작성**

   **Frontmatter**:
   ```yaml
   ---
   name: {skill-name}
   description: {언제 사용하는지 명시}. {구체적 키워드 포함}.
   ---
   ```

   **핵심 철학** (선택, 여러 방법 중 선택 시):
   ```markdown
   **핵심 철학**:
   - 원칙 1
   - 원칙 2
   - 원칙 3
   - 원칙 4
   ```

   **Instructions**:
   - 워크플로우 기반: ### 워크플로우 1, 2, 3...
   - Phase 기반: ## Phase 1, 2, 3...
   - 리소스 로딩: 키워드 매칭 테이블 + 리소스 로딩 전략

   **중요 원칙** (3-7개):
   ```markdown
   ## 중요 원칙

   1. **원칙명**: 설명
   2. **원칙명**: 설명
   ```

   **Examples** (2-3개, 플로우 차트식):
   ```markdown
   ## Examples

   ### {시나리오명}
   User: "{요청}" → 워크플로우 N → {결과}
   ```

   **Technical Details**:
   ```markdown
   ## Technical Details

   상세한 내용은 `REFERENCE.md`를 참조하세요.
   ```

5. **추가 파일 생성** (선택)

   **REFERENCE.md** (리소스 5개 이상 시):
   - 리소스 전체 개요
   - 권장 학습 순서
   - 트러블슈팅

   **templates/** (파일 생성 skill):
   - 템플릿 파일들
   - Frontmatter 포함

   **resources/** (지식 분리 시):
   - 번호 매김 (01-05)
   - 주제별 분리
   - 언어별 분리 (languages/)

6. **검증**
   - Best practices 체크리스트 (`resources/02-best-practices-checklist.md`)
   - 분량 검증 (유형별 권장 범위)
   - 필수 섹션 확인

7. **Git 커밋**
   ```bash
   cd ~/dots/prompts/skills/{skill-name}
   git add .
   git commit -m "[skill] add {skill-name}"
   ```

8. **사용법 안내**
   - Skill 호출 방법
   - 키워드 (description에서 추출)
   - 예시 요청 문장

## 중요 원칙

1. **컨텍스트 우선**: 대화와 파일 변경을 자동 분석, 사용자 입력 최소화
2. **사용자 확인**: 각 주요 결정 지점에서 사용자 확인 (갱신 vs 신규, skill 유형)
3. **Best practices 자동 적용**: 기존 skills 패턴 재사용, 일관성 유지
4. **토큰 효율**: SKILL.md는 핵심만, 상세 내용은 resources/로 분리
5. **검증 자동화**: 체크리스트 기반, 분량/파일명/구조 자동 확인

## 안티패턴

| ❌ 문제 | ✅ 해결 |
|--------|--------|
| SKILL.md 500줄 초과 | resources/로 상세 내용 분리 |
| 프로젝트 특정 정보 포함 | placeholder 사용 ({project}, {name}) |
| 다른 skill과 내용 중복 | 참조로 대체 ("tdd-practices 참조") |
| description에 키워드 부족 | 트리거 키워드 명시적 포함 |

## Examples

### 기존 Skill 갱신
```
User: "여태까지 작업을 ldoc에 반영해줘"
→ 워크플로우 1: 작업 분석
→ 워크플로우 2: ldoc 발견 (높은 연관성)
→ 워크플로우 3A: 갱신 실행
→ Git 커밋
```

### 신규 Skill 생성
```
User: "이 작업 패턴을 skill로 만들어줘"
→ 워크플로우 1: 작업 분석
→ 워크플로우 2: 관련 skill 없음
→ 워크플로우 3B: 신규 생성 (유형 선택 → 작성 → 검증)
→ 사용법 안내
```

### Skill 확장/리네임 (proactive 트리거)
```
User: "claude-guide skill 확장해줘" 또는 "ldoc에 워크플로우 추가해줘"
→ 자동 트리거: skill 이름 + 작업 동사 감지
→ 워크플로우 3A: 기존 skill 갱신
→ Best practices 검증
```

## Technical Details

상세한 템플릿과 체크리스트는 다음을 참조하세요:
- `REFERENCE.md`: Skill 유형별 상세 설명
- `resources/01-type-templates.md`: 5가지 유형 템플릿
- `resources/02-best-practices-checklist.md`: 검증 체크리스트
- `resources/03-update-patterns.md`: 갱신 패턴 가이드
- `templates/SKILL-template.md`: 기본 뼈대
