---
name: devops-pipelines
description: Configures CI/CD pipelines for GitHub Actions or GitLab CI. Use when setting up automated build, test, deploy workflows.
---

# CI/CD Pipelines

GitHub Actions, GitLab CI 등 CI/CD 파이프라인을 구성합니다.

**핵심 철학**:
- 로컬과 CI 환경 동일하게 (justfile 재사용)
- 캐싱으로 빌드 시간 최적화
- Service container로 통합 테스트
- 환경별 배포 분리

## Instructions

### 워크플로우: CI/CD 파이프라인 설정

1. **프로젝트 분석**
   - Git 호스팅 확인 (GitHub/GitLab)
   - 기존 워크플로우 확인

2. **리소스 로드**
   - Read resources/02-ci-cd-pipelines.md

3. **설정 생성**
   - .github/workflows/ci.yml 또는 .gitlab-ci.yml
   - 캐싱 설정
   - Service container 설정

4. **검증**
   - YAML 문법 검증
   - dry-run 가능 시 실행

## 키워드 매칭

| 키워드 | 동작 |
|--------|------|
| CI/CD, 파이프라인 | 전체 워크플로우 |
| GitHub Actions | .github/workflows/ |
| GitLab CI | .gitlab-ci.yml |
| 캐싱, cache | 의존성 캐시 패턴 |

## Examples

### GitHub Actions 설정
User: "GitHub Actions로 CI/CD 파이프라인 만들어줘"
→ Read 02-ci-cd-pipelines.md
→ .github/workflows/ci.yml 생성
→ 캐싱 + service container 설정

## Technical Details

- `resources/02-ci-cd-pipelines.md`: GitHub Actions, GitLab CI 패턴
