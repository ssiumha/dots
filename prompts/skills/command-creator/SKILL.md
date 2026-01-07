---
name: command-creator
description: 반복 워크플로우를 slash command로 정의할 때 사용. 배치 처리, 파이프라인, 체크리스트 기반 자동화를 지원합니다. (user)
---

# Command Creator

Slash command를 생성하고 관리합니다. Command는 반복 작업을 단축키처럼 호출하는 프롬프트 템플릿입니다.

## 파일 위치

| 위치 | 경로 | 용도 |
|------|------|------|
| 프로젝트 | `.claude/commands/{name}.md` | 현재 프로젝트에서만 사용 |
| 전역 | `~/.claude/commands/{name}.md` | 모든 프로젝트에서 사용 |

## Command 유형

| 유형 | 설명 | 템플릿 | 예시 |
|------|------|--------|------|
| simple | 단순 작업 실행 | `templates/simple.md` | `/format`, `/lint` |
| batch-processor | 체크리스트 + 병렬 처리 | `templates/batch-processor.md` | `/img-batch`, `/migrate` |
| interactive | 질문-응답 기반 | `templates/interactive.md` | `/init`, `/setup` |
| pipeline | 순차 단계 처리 | `templates/pipeline.md` | `/deploy`, `/release` |

## Creation Process

### 1. 요구사항 수집

AskUserQuestion으로 확인:

**[필수]**
- **목적**: 이 command가 무엇을 하는가?
- **입력**: `$ARGUMENTS`로 받을 값은?

**[선택]**
- **유형**: 단순 / 배치 / 대화형 / 파이프라인?
- **병렬 처리**: agent 병렬 호출 필요?

### 2. 기존 command 확인

중복 방지를 위해 검색:
```bash
Glob ~/.claude/commands/*.md
Glob .claude/commands/*.md
```

### 3. 유형 선택

사용자 요구사항 기반 자동 추천:

| 키워드 | 추천 유형 |
|--------|----------|
| "배치", "병렬", "체크리스트" | batch-processor |
| "단계별", "순차", "파이프라인" | pipeline |
| "질문", "대화", "선택" | interactive |
| 그 외 | simple |

### 4. 위치 선택

- [1] 프로젝트: `.claude/commands/{name}.md`
- [2] 전역: `~/.claude/commands/{name}.md`

### 5. 템플릿 기반 생성

1. `templates/{type}.md` Read
2. 플레이스홀더 치환
3. Write로 파일 생성

## Modification Process

1. **대상 확인**: 기존 command 파일 Read
2. **수정 유형 파악**:
   - 로직 변경 → 본문 수정
   - 인자 추가 → `$ARGUMENTS` 설명 수정
   - 병렬화 추가 → batch-processor 패턴 적용
3. **Edit으로 수정**

## 중요 원칙

1. **$ARGUMENTS 활용**: 사용자 입력은 `$ARGUMENTS`로 받음
2. **명확한 단계**: 번호 매김으로 순서 명시
3. **에러 처리**: 실패 시 동작 정의
4. **상태 관리**: 배치 처리 시 체크리스트 파일 활용

## Examples

### Simple command 생성

**User**: "코드 포맷팅 command 만들어줘"

**Flow**:
1. 유형: simple
2. 위치: 프로젝트
3. `templates/simple.md` 기반 생성
4. `.claude/commands/format.md` 작성

### Batch processor 생성

**User**: "이미지를 컴포넌트로 변환하는 배치 command 만들어줘"

**Flow**:
1. 유형: batch-processor (병렬 + 체크리스트)
2. 위치: 프로젝트
3. `templates/batch-processor.md` 기반 생성
4. `.claude/commands/img-batch.md` 작성

**사용**: `/img-batch checklist.yaml`

### 기존 command 수정

**User**: "format command에 lint도 추가해줘"

**Flow**:
1. 기존 파일 Read
2. 단계 추가
3. Edit으로 수정

## Technical Details

상세 템플릿은 다음 파일 참조:
- `templates/simple.md`: 단순 작업
- `templates/batch-processor.md`: 체크리스트 + 병렬 처리
- `templates/interactive.md`: 질문-응답 기반
- `templates/pipeline.md`: 순차 단계 처리
- `resources/01-command-types.md`: 유형별 상세 설명
