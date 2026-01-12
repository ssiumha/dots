---
name: devops-docker
description: Creates Docker configurations and compose files. Use when containerizing apps, setting up Docker dev environment, writing Dockerfile/compose.yaml, or building multi-stage images.
---

# Docker

Docker 기반 개발 환경 및 컨테이너 이미지를 구성합니다.

**핵심 철학**:
- compose.yaml (docker compose v2 표준)
- 멀티스테이지 빌드로 이미지 최적화
- .dockerignore로 빌드 컨텍스트 최소화
- 개발/운영 환경 분리

## Instructions

### 워크플로우: Docker 환경 구축

1. **프로젝트 분석**
   - 언어/프레임워크 확인
   - 기존 Docker 설정 확인

2. **리소스 로드**
   - Read resources/03-docker.md

3. **설정 생성**
   - Dockerfile (멀티스테이지)
   - compose.yaml (app + 의존 서비스)
   - .dockerignore

4. **검증**
   - docker compose build
   - docker compose up

## 키워드 매칭

| 키워드 | 동작 |
|--------|------|
| Docker, Dockerfile | 이미지 빌드 패턴 |
| compose, compose.yaml | 서비스 오케스트레이션 |
| 컨테이너, container | 컨테이너화 가이드 |
| 멀티스테이지, multi-stage | 최적화 빌드 |

## Examples

### 개발 환경 Docker화
User: "Docker로 개발 환경 구축해줘"
→ Read 03-docker.md
→ Dockerfile + compose.yaml + .dockerignore 생성
→ docker compose up 확인

## Technical Details

- `resources/03-docker.md`: Dockerfile, compose.yaml 패턴
