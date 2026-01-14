# skills 필드 상세

Subagent가 초기화될 때 자동으로 로드할 skill 목록을 지정합니다.

## 용도

- 특정 도메인 지식을 subagent에 자동 주입
- 반복적인 프롬프트를 skill로 캡슐화하여 재사용
- 여러 skill을 조합한 전문화된 agent 구성

## 동작 방식

1. Subagent 생성 시 `skills:` 목록의 각 skill을 로드
2. 해당 skill의 SKILL.md 내용이 subagent 컨텍스트에 통합
3. Subagent는 skill의 지식과 워크플로우를 활용 가능

## 예시

```yaml
---
name: code-reviewer
description: Reviews code for quality and security
model: sonnet
tools:
  - Read
  - Glob
  - Grep
skills:
  - review-security      # 보안 취약점 검토 skill
  - quality-audit        # 코드 품질 감사 skill
---
```

## 주의사항

- Skill 이름은 사용 가능한 skill 목록에서 선택 (Available skills 참조)
- 너무 많은 skill 로드 시 컨텍스트 소모 증가
- 관련 없는 skill은 오히려 노이즈가 될 수 있음
- 권장: 1-3개의 밀접하게 관련된 skill만 포함 (각 skill ~5-10KB 컨텍스트 소모, subagent 컨텍스트 한계 고려)
