# DevOps Patterns Reference

patterns-devops skill의 리소스 전체 개요 및 빠른 참조 가이드입니다.

## 리소스 구조

```
resources/
├── 01-local-ci.md           # 우선순위 1: 로컬 CI 자동화
├── 02-ci-cd-pipelines.md    # 우선순위 2: CI/CD 파이프라인
├── 03-docker.md             # 우선순위 3: Docker 환경
├── 04-deployment.md         # 우선순위 4: 배포 자동화
└── languages/
    ├── typescript.md        # TypeScript: Biome + Vitest
    ├── python.md            # Python: Ruff + pytest (향후)
    └── ruby.md              # Ruby: RuboCop + RSpec (향후)
```

## 각 리소스 개요

### 01-local-ci.md (로컬 CI)

**목적**: 로컬 개발 환경에서 lint와 test를 자동으로 실행

**포함 내용**:
- Justfile 패턴 및 베스트 프랙티스
- E2E 테스트 전략 (compose.yaml로 DB 컨테이너 띄우기)
- pre-commit hooks 통합
- 공통 태스크 패턴 (lint, test, format, build)

**사용 시점**:
- 프로젝트 초기 설정
- 로컬 개발 환경 자동화
- 팀 컨벤션 통일

**의존성**: languages/{언어}.md (언어별 도구 상세)

---

### 02-ci-cd-pipelines.md (CI/CD)

**목적**: GitHub Actions, GitLab CI 등 CI/CD 파이프라인 설정

**포함 내용**:
- GitHub Actions 워크플로우 패턴
- GitLab CI 파이프라인 패턴
- compose.yaml 기반 service container
- 매트릭스 빌드 (여러 버전 테스트)
- 캐싱 전략 (의존성, 빌드 캐시)
- 브랜치별 전략 (main, PR, tag)

**사용 시점**:
- CI/CD 파이프라인 구축
- 자동 테스트 및 빌드
- 멀티 환경 테스트

**의존성**: 01-local-ci.md (로컬 CI 패턴 재사용)

---

### 03-docker.md (Docker)

**목적**: Docker 컨테이너 환경 구축

**포함 내용**:
- Dockerfile 베스트 프랙티스
- 멀티스테이지 빌드 패턴
- compose.yaml 구성 (v2 spec)
- 개발/프로덕션 환경 분리
- 이미지 최적화 (레이어 캐싱, 크기 최소화)
- .dockerignore 패턴
- 헬스체크 설정

**사용 시점**:
- Docker 환경 구축
- 로컬 개발 환경 컨테이너화
- E2E 테스트용 DB 설정

**의존성**: 없음 (독립적)

---

### 04-deployment.md (배포)

**목적**: 서비스 배포 및 프로세스 관리

**포함 내용**:
- 배포 스크립트 패턴
- PM2 통합 및 설정
- 서비스 재시작 전략
- 롤백 메커니즘
- 환경변수 관리
- 헬스체크 및 모니터링
- Zero-downtime deployment

**사용 시점**:
- 서비스 배포 자동화
- 프로세스 관리
- 운영 환경 구축

**의존성**: 03-docker.md (Docker 배포 시)

---

### languages/typescript.md

**목적**: TypeScript 프로젝트의 lint 및 test 도구 상세

**포함 내용**:
- Biome 설정 및 사용법 (ESLint 대체)
- Vitest/Jest 설정
- 타입 체크 (tsc --noEmit)
- 테스트 커버리지
- E2E 테스트 패턴
- Justfile 통합 예제

**사용 시점**:
- TypeScript 프로젝트 설정
- Biome으로 마이그레이션
- 테스트 환경 구축

**의존성**: 01-local-ci.md (통합 패턴)

---

## 우선순위 가이드

### 신규 프로젝트
1. **01-local-ci.md** + **languages/{언어}.md**: 로컬 개발 환경 먼저
2. **02-ci-cd-pipelines.md**: CI/CD 파이프라인 구축
3. **03-docker.md**: 필요 시 Docker 환경
4. **04-deployment.md**: 배포 필요 시

### 기존 프로젝트 개선
1. **01-local-ci.md**: 로컬 CI 개선
2. **03-docker.md**: E2E 테스트용 DB 컨테이너
3. **02-ci-cd-pipelines.md**: CI/CD 최적화
4. **04-deployment.md**: 배포 자동화

### 배포 프로젝트
1. **04-deployment.md**: 배포 스크립트
2. **03-docker.md**: Docker 배포 환경
3. **02-ci-cd-pipelines.md**: CD 파이프라인
4. **01-local-ci.md**: 로컬 개발 개선

## 리소스 간 관계

```
01-local-ci.md
├── languages/typescript.md (언어별 도구)
└── 03-docker.md (E2E 테스트용 DB)

02-ci-cd-pipelines.md
├── 01-local-ci.md (로컬 CI 패턴 재사용)
├── 03-docker.md (service container)
└── languages/typescript.md (빌드/테스트)

03-docker.md
└── (독립적)

04-deployment.md
└── 03-docker.md (Docker 배포 시)
```

## 빠른 참조

### 키워드 → 리소스 매핑

- **Justfile, lint, test** → 01-local-ci.md
- **TypeScript, Biome** → languages/typescript.md
- **GitHub Actions, CI/CD** → 02-ci-cd-pipelines.md
- **Docker, compose** → 03-docker.md
- **배포, PM2** → 04-deployment.md

### 일반적인 워크플로우

#### 신규 TypeScript 프로젝트
1. Read 01-local-ci.md
2. Read languages/typescript.md
3. justfile 생성 (lint, test)
4. biome.json 생성
5. 실행 확인: just lint, just test

#### CI/CD 추가
1. Read 02-ci-cd-pipelines.md
2. Read languages/typescript.md (확인)
3. .github/workflows/ci.yml 생성
4. 로컬 패턴 재사용
5. Push 및 확인

#### Docker 환경
1. Read 03-docker.md
2. Dockerfile 생성 (멀티스테이지)
3. compose.yaml 생성
4. 로컬 실행: docker compose up
