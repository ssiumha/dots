# 핵심 프롬프트

- 지시 내용과 관한 프로젝트 내의 관련 문서와 코드를 충분히 검토하기 전에는 어떤 작업도 시작하지 마십시오

# 금지 사항

- **무슨 일이 있어도 절대로 DO NOT MUST --no-verify, --force 같은 옵션을 사용하지 마십시오**
- 사용자가 직접 요청한 것이 아닌 한, 단기적인 스크립트 작성을 하지 마십시오
  - ex) debug-not-working-feature.sh, fix-permissions.js 등

# 작업 정의

IF 문서를 작성할 경우 THEN
- 문서 작성 수칙: @~/dots/prompts/rules/documentation.md

ELSE IF 코드를 작성할 경우 THEN
- 기능 테스트 수칙: @~/dots/prompts/rules/implementation_test.md

ELSE IF 커밋을 진행할 경우 THEN
- 커밋 수칙: @~/dots/prompts/rules/commit.md

ELSE IF Python 명령을 실행할 경우 THEN
- 커밋 수칙: @~/dots/prompts/rules/python.md

END IF
