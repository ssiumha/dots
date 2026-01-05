ldoc skill을 실행하여 프로젝트 지식을 문서화합니다.

## 사용 방법

```
/docs
```

## 수행 작업

이 명령은 ldoc skill을 활성화하여 다음을 수행합니다:

- **의사결정 기록 (ADR)**: 중요한 기술적 결정사항 문서화
- **TODO 항목 관리**: 작업 목록 생성 및 업데이트
- **지식베이스 업데이트**: 아키텍처, 패턴, 주의사항 기록
- **요구사항 문서화**: 기능 명세 및 정책 문서 작성

## 자동 프로젝트 인식

ldoc skill은 현재 작업 디렉토리를 기반으로 프로젝트를 자동 인식합니다.

문서는 `~/docs/{project}/` 디렉토리에 저장됩니다:
- `decisions/`: ADR 문서
- `knowledge/`: 지식베이스 문서
- `todos/`: TODO 목록

## 참고

- 자세한 사용법: ldoc skill 참조
- 템플릿 구조: `~/dots/prompts/skills/ldoc/templates/`
