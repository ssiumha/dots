---
name: devops-local-ci
description: Sets up local CI with Justfile task runner. Use when setting up lint/test/format automation, writing pre-commit hooks, or running E2E tests with containers.
---

# Local CI

로컬 개발 환경에서 lint, test, format을 Justfile로 통합 자동화합니다.

**핵심 철학**:
- Justfile로 모든 로컬 자동화 통합
- E2E 테스트: 실제 DB 컨테이너 사용
- 언어별 최신 도구 (TypeScript: Biome)
- pre-commit hooks로 품질 게이트

## Instructions

### 워크플로우: Local CI 설정

1. **프로젝트 분석**
   - 언어/프레임워크 확인
   - 기존 lint/test 설정 확인

2. **리소스 로드**
   - Read resources/01-local-ci.md
   - 언어별: Read resources/languages/{lang}.md

3. **설정 생성**
   - justfile 생성/수정
   - 언어별 lint 설정 (biome.json 등)
   - E2E용 compose.yaml

4. **검증**
   - just lint, just test 실행 확인

## 키워드 매칭

| 키워드 | 리소스 |
|--------|--------|
| lint, formatter, biome | 01-local-ci.md |
| test, E2E, 통합테스트 | 01-local-ci.md |
| justfile, task runner | 01-local-ci.md |
| pre-commit, hooks | 01-local-ci.md |
| TypeScript + lint/test | languages/typescript.md |

## Examples

### TypeScript 프로젝트 설정
User: "TypeScript 프로젝트에 lint, test 자동화"
→ Read 01-local-ci.md + languages/typescript.md
→ justfile, biome.json 생성
→ just lint, just test 확인

## Technical Details

- `resources/01-local-ci.md`: Justfile 패턴, E2E 테스트
- `resources/languages/typescript.md`: Biome, Vitest/Jest
