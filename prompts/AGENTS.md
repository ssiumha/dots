# 핵심 프롬프트

- 지시 내용과 관한 프로젝트 내의 관련 문서와 코드를 충분히 검토하기 전에는 어떤 작업도 시작하지 마십시오

# 금지 사항

- **무슨 일이 있어도 절대로 DO NOT MUST --no-verify, --force 같은 옵션을 사용하지 마십시오**
- 사용자가 직접 요청한 것이 아닌 한, 단기적인 스크립트 작성을 하지 마십시오
  - ex) debug-not-working-feature.sh, fix-permissions.js 등

# 자율 개발 모드

/auto-dev 스킬이 활성화된 경우:

- **완료 기준**: 테스트 통과 + PR 생성 (이전까지 멈추지 않음)
- **SPECIFY 필수**: 구현 전 요구사항을 질문으로 확정
- **막힘 시**: 3회 자체 해결 시도 → 4회차에 사용자 질문

상세 워크플로우는 auto-dev SKILL.md 참조.

# Skill-Local Subagent 패턴

스킬 내부에 전용 서브에이전트를 정의하여 컨텍스트를 분리합니다.

## 구조

```
prompts/skills/{skill-name}/
├── SKILL.md
├── agents/                    # 스킬 전용 에이전트
│   └── {agent-name}.md
└── resources/
    └── {detailed-spec}.md     # 상세 스펙 (유지)
```

## 호출 패턴 (Read + Task)

1. 에이전트 파일 Read
2. Task 호출 시 에이전트 내용을 prompt에 포함
3. `subagent_type="general-purpose"` 사용

```markdown
### 워크플로우 X: {작업명}

**에이전트 호출**:
1. `agents/{agent-name}.md` Read
2. Task(subagent_type="general-purpose", prompt="{파일 내용}\n\n{작업 컨텍스트}")
3. 결과 기반 후속 처리
```

## 장점

- **캡슐화**: 스킬 관련 에이전트가 스킬 폴더 내에 위치
- **컨텍스트 분리**: 필요할 때만 에이전트 프롬프트 로드
- **유지보수**: 에이전트 로직 변경 시 해당 파일만 수정
- **재사용**: 다른 스킬에서 Read로 참조 가능

## 적용 사례

- `ldoc/agents/health-checker.md`: 문서 건강도 검사 (WF10)
