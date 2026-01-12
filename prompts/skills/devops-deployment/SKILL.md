---
name: devops-deployment
description: Manages deployment, infrastructure, and observability. Use when deploying with PM2, Helmfile/K8s, configuring databases, or setting up Prometheus/Grafana monitoring.
---

# Deployment & Operations

배포, 인프라, 모니터링을 포함한 운영 환경을 구성합니다.

**핵심 철학**:
- 점진적 롤아웃, 빠른 롤백
- 환경별 설정 분리 (dev/staging/prod)
- 관측성(Observability) 기본 탑재
- 보안 베이스라인 적용

## Instructions

### 워크플로우: 요청 분석 및 리소스 선택

사용자 요청의 키워드를 분석하여 필요한 리소스만 로드합니다.

#### 키워드 매칭

| 키워드 | 리소스 |
|--------|--------|
| 배포, deploy, PM2 | 04-deployment.md |
| postgres, mysql, DB | 05-database.md |
| helmfile, k8s, kubernetes | 06-helmfile-k8s.md |
| prometheus, grafana, 모니터링 | 07-observability.md |

#### 리소스 적용

1. **환경 파악**: 타겟 환경 확인 (VM, K8s, 컨테이너)
2. **리소스 로드**: 키워드 매칭으로 필요 리소스 Read
3. **설정 생성**: 환경에 맞는 설정 파일 생성
4. **검증**: dry-run, diff로 변경 사항 확인

## Examples

### PM2 배포
User: "PM2로 Node 앱 배포 설정"
→ Read 04-deployment.md
→ ecosystem.config.js 생성
→ 배포 스크립트 작성

### Helmfile K8s 배포
User: "Helmfile로 K8s 배포 설정해줘"
→ Read 06-helmfile-k8s.md
→ helmfile.yaml + values/ 생성
→ helmfile -e dev diff 확인

### Observability 구축
User: "Prometheus, Grafana 모니터링 설정"
→ Read 07-observability.md
→ prometheus.yml, dashboard 설정
→ actuator 엔드포인트 확인

## Technical Details

- `resources/04-deployment.md`: PM2, 배포 스크립트
- `resources/05-database.md`: PostgreSQL/MySQL 설정
- `resources/06-helmfile-k8s.md`: Helmfile, K8s 보안
- `resources/07-observability.md`: Prometheus, Grafana, OpenTelemetry
