# 핵심 원칙

- **IMPORTANT**: 관련 문서/코드 검토 전 작업 시작 금지
- Research First: 조사 → 계획 → 구현
- 수동 작업 언급 시 자동화 제안
- 간결한 설명 우선 (장황함 지양)

# 지속적 개선

반복/지적/방향수정 = 컨텍스트 문제 신호

- 같은 지적 2회 → CLAUDE.md/rules 추가 제안
- 같은 질문 반복 → 프롬프트 개선 제안
- 방향 수정 빈번 → 컨텍스트 부족, 파일 참조 제안

# Anti-patterns

- 테스트 없이 "동작 확인됨" 금지
- 파일 읽기 전 내용 추측 금지
- 에러 시 원인 분석 없이 수정 금지
- 기존 코드 제거 전 이유 파악 (Chesterton's Fence)
- 사용자 확인 없이 범위 확장 금지

# Context Engineering

Context는 유한. 최소 토큰으로 최대 효과.

- 파일에 저장 후 필요 시 Read (외부 메모리)
- subagent 작업 후 핵심만 반환 (1-2K 토큰)
- 자주 참조하는 정보는 전용 파일로 분리

# Subagent 활용

Subagent = 컨텍스트 방화벽. 격리 실행 후 요약만 반환.

**즉시 위임**: 500줄+ 파일, 3+ 파일 탐색, 웹 검색, 로그 분석, 대화 초반 조사

**Proactive 호출**:
- 2+ 파일 수정 → code-reviewer
- 작업 완료 → ldoc-automation
- 페이즈 완료 → tidy-commit

**대규모 작업**: Explore(구조파악) → Plan(계획) → Execute(분할처리) → Verify(통합검증)

# 모델 선택

- 탐색/검색: haiku
- 일반 작업: sonnet
- 복잡한 설계: opus

# 자동화 단계

동일 패턴 반복 시: 2회 → script, 3회 → hook/skill, 5회+ → agent

# 병렬 실행

의존성 없으면 병렬. 독립 작업 3+ 시 subagent 병렬 (최대 10개).

# 금지

- **MUST NOT**: `--no-verify`, `--force` 옵션 사용 금지
- 일회성 디버깅 스크립트 작성 금지

# 참조

- `/auto-dev`: 자율 개발 모드 (SPECIFY → 테스트 → PR)
- `agent-creator`: skill을 subagent로 자동화
