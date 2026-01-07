# JD 영역 카탈로그

각 영역(Area)의 상세 설명 및 사용 가이드

## 00-09 System

**목적**: 문서 시스템 자체 관리

| 카테고리 | 용도 | 예시 |
|----------|------|------|
| 00-Index | JDex (마스터 인덱스) | 00.00-jdex.md |
| 01-Templates | 문서 템플릿 | 01.01-adr.md |
| 02-Guidelines | 작성 가이드라인 | 02.01-writing-guide.md |

**권장**: 모든 프로젝트 필수

---

## 10-19 Overview

**목적**: 프로젝트 전반 소개

| 카테고리 | 용도 | 예시 |
|----------|------|------|
| 11-README | 프로젝트 소개, 퀵스타트 | 11.01-readme.md |
| 12-Roadmap | 로드맵, 마일스톤 | 12.01-2024-roadmap.md |
| 13-Changelog | 변경 이력, 릴리스 노트 | 13.01-v2-release.md |
| 14-Goals | 프로젝트 목표, 비전 | 14.01-vision.md |

**권장**: 오픈소스, 팀 프로젝트

---

## 20-29 Architecture

**목적**: 아키텍처 결정 및 설계

| 카테고리 | 용도 | 예시 |
|----------|------|------|
| 21-ADR | Architecture Decision Records | 21.01-database-selection.md |
| 22-System-Design | 시스템 설계 문서 | 22.01-auth-flow.md |
| 23-Data-Model | 데이터 모델, ERD | 23.01-user-schema.md |
| 24-Diagrams | 아키텍처 다이어그램 | 24.01-system-overview.md |
| 25-RFC | 기술 제안서 | 25.01-microservices.md |

**권장**: 모든 프로젝트 (핵심)

---

## 30-39 API

**목적**: API 및 인터페이스 문서

| 카테고리 | 용도 | 예시 |
|----------|------|------|
| 31-REST-API | REST API 명세 | 31.01-user-api.md |
| 32-GraphQL | GraphQL 스키마 | 32.01-query-schema.md |
| 33-Events | 이벤트/메시지 스키마 | 33.01-user-events.md |
| 34-SDK | SDK/클라이언트 문서 | 34.01-js-sdk.md |
| 35-Integration | 외부 연동 가이드 | 35.01-stripe-integration.md |

**권장**: API 제공 프로젝트

---

## 40-49 Development

**목적**: 개발자 가이드

| 카테고리 | 용도 | 예시 |
|----------|------|------|
| 41-Setup | 환경 설정, 설치 가이드 | 41.01-local-setup.md |
| 42-Conventions | 코드 컨벤션, 스타일 가이드 | 42.01-code-style.md |
| 43-Contributing | 기여 가이드 | 43.01-contributing.md |
| 44-Testing | 테스트 가이드 | 44.01-testing-guide.md |
| 45-Security | 보안 가이드라인 | 45.01-security-practices.md |

**권장**: 팀 프로젝트, 오픈소스

---

## 50-59 Process

**목적**: 프로세스 및 협업

| 카테고리 | 용도 | 예시 |
|----------|------|------|
| 51-Requirements | 요구사항 명세 | 51.01-user-auth-req.md |
| 52-Tasks | 작업 관리, 스프린트 | 52.01-sprint-1.md |
| 53-Meetings | 회의록 | 53.01-kickoff-meeting.md |
| 54-Retrospectives | 회고, 포스트모템 | 54.01-sprint-1-retro.md |
| 55-Decisions | 비기술적 의사결정 | 55.01-pricing-decision.md |

**권장**: 팀 프로젝트

---

## 60-69 Operations

**목적**: 운영 및 인프라

| 카테고리 | 용도 | 예시 |
|----------|------|------|
| 61-Deployment | 배포 가이드, 파이프라인 | 61.01-deploy-guide.md |
| 62-Monitoring | 모니터링, 알림 설정 | 62.01-alerting-setup.md |
| 63-Incidents | 인시던트 기록 | 63.01-server-outage.md |
| 64-Runbooks | 운영 절차서 | 64.01-db-failover.md |
| 65-SLA | SLA, SLO 정의 | 65.01-api-sla.md |

**권장**: 프로덕션 운영 프로젝트

---

## 70-79 Knowledge

**목적**: 팀 지식 축적

| 카테고리 | 용도 | 예시 |
|----------|------|------|
| 71-Troubleshooting | 트러블슈팅 가이드 | 71.01-connection-timeout.md |
| 72-Learnings | 학습 내용, TIL | 72.01-react-hooks.md |
| 73-Best-Practices | 베스트 프랙티스 | 73.01-error-handling.md |
| 74-Patterns | 사용 패턴, 레시피 | 74.01-caching-pattern.md |
| 75-FAQ | 자주 묻는 질문 | 75.01-onboarding-faq.md |

**권장**: 장기 운영 프로젝트

---

## 80-89 Reference

**목적**: 참조 자료

| 카테고리 | 용도 | 예시 |
|----------|------|------|
| 81-External-Docs | 외부 문서 링크 | 81.01-aws-docs.md |
| 82-Glossary | 용어집 | 82.01-glossary.md |
| 83-Dependencies | 의존성 문서 | 83.01-dependencies.md |
| 84-Licenses | 라이선스 정보 | 84.01-licenses.md |

**권장**: 선택적

---

## 90-99 Archive

**목적**: 보관용 문서

| 카테고리 | 용도 | 예시 |
|----------|------|------|
| 91-Deprecated | 폐기된 문서 | (이동됨) |
| 92-Old-Versions | 과거 버전 | (이동됨) |
| 93-Migration | 마이그레이션 기록 | 93.01-v1-to-v2.md |

**권장**: 선택적

---

## 권장 세트

### 최소 (소규모 프로젝트)

```
00-09-System
20-29-Architecture
40-49-Development
```

### 표준 (팀 프로젝트)

```
00-09-System
20-29-Architecture
30-39-API
40-49-Development
50-59-Process
```

### 전체 (엔터프라이즈)

```
모든 영역
```
