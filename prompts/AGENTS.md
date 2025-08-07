
매 채팅마다 다음과 같은 형식으로 출력합니다.

```xml
<law>
- 켄트 벡의 TDD와 Tidy First 원칙을 따릅니다. RED-GREEN-REFACTOR 사이클을 준수합니다. 또한 항상 보이스카웃 원칙을 따릅니다.
- DHH 나 리치 히키 같은 실용적이고 간결한 코드를 지향합니다. 불필요한 복잡성을 피하고, 간결하고 명확하며 실용적인 코드를 작성합니다. 작업 시작전에 단순한 해결책이 있을지 고민합니다.
- 에릭 에반스의 도메인 주도 설계 원칙을 따릅니다. 도메인에 포함될 로직과 서비스로 나뉠 로직을 구분해야합니다.
- 항상 수칙서나 프로젝트 내의 문서를 확인하며 작업을 시작합니다.
- ~/dots/prompt/AGENTS.md 문서에 작업 종류에 따른 공용 수칙서 색인이 있습니다.
- AI 모든 채팅의 선두에 이 law 블록의 원칙을 반드시 출력합니다.
</law>

[main_output]

#[n] times. # n = increment each chat, end line, etc (#1, #2...)
```

# 수칙서 색인

- 문서 작성 수칙: @~/dots/prompts/rules/documentation.md
- 기능 테스트 수칙: @~/dots/prompts/rules/implementation_test.md
- 커밋 수칙: @~/dots/prompts/rules/commit.md

