# Skill 호출 규칙

Available skills에 있는 skill 이름이 **실행 의도와 함께** 언급되면 Skill 도구를 호출합니다.

## 호출 트리거

다음 패턴 충족 시 호출:

| 패턴 | 예시 |
|------|------|
| `{skill} + 동사` | "auto-dev 실행", "ldoc 써서", "skill-creator로 만들어" |
| `X를 {skill}으로` | "이 코드를 ldoc으로 문서화" |
| `{skill} 단독 명령` | "auto-dev" (문맥상 실행 의도 명확) |
| `{skill} + 조사형` | "skill-creator로 할까?", "이걸 ldoc으로?" |

## 호출하지 않는 케이스

| 유형 | 예시 |
|------|------|
| **정보 질문** | "skill-creator가 뭐야?", "어떻게 써?" |
| **비교/분석** | "ldoc이랑 dev-docs 차이", "어떤 걸 쓸까?" |
| **부정/거부** | "auto-dev는 쓰지 마", "필요 없어" |
| **미래/계획** | "나중에 skill-creator 써볼게" |
| **우연한 언급** | "auto-dev 같은 도구", "skill-creator처럼" (비유/비교 대상) |
| **파일 직접 수정** | "{skill}/SKILL.md 수정해줘" → Edit 사용 |

## Skill vs Subagent 우선순위

| 작업 | 도구 |
|------|------|
| Skill 고유 워크플로우 | Skill 도구 |
| 일반 코드 작업 | subagent 위임 (CLAUDE.md 규칙) |
| Skill 파일 직접 수정 | Edit/Write |

Skill 내부에서 subagent 위임은 허용 (auto-dev → Task 등).
