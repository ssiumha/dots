# whitepaper-kr 리소스 레퍼런스

한국어 기술 백서 작성 Skill의 리소스 구조와 사용법입니다.

## 리소스 구조

```
whitepaper-kr/
├── SKILL.md              # 핵심 워크플로우
├── REFERENCE.md          # 이 파일
├── resources/
│   ├── 01-overview.md         # 개요 섹션 가이드
│   ├── 02-solution.md         # 해결책 섹션 가이드
│   ├── 03-architecture.md     # 기술 아키텍처 가이드
│   ├── 04-differentiation.md  # 차별화 포인트 가이드
│   ├── 05-team.md             # 팀 소개 가이드
│   ├── 06-roadmap.md          # 로드맵 가이드
│   ├── 07-business-model.md   # 비즈니스 모델 가이드 (선택)
│   ├── 08-tokenomics.md       # 토크노믹스 가이드 (블록체인)
│   └── 09-legal.md            # 법적 고지 가이드 (선택)
└── templates/
    ├── whitepaper-general.md     # 일반 기술 백서 템플릿
    ├── whitepaper-ai.md          # AI 프로젝트 템플릿
    └── whitepaper-blockchain.md  # 블록체인 프로젝트 템플릿
```

## 프로젝트 유형별 사용 리소스

| 섹션 | 일반 | AI | 블록체인 |
|------|:----:|:--:|:--------:|
| 01 개요 | ✓ | ✓ | ✓ |
| 02 해결책 | ✓ | ✓ | ✓ |
| 03 아키텍처 | ✓ | ✓ | ✓ |
| 04 차별화 | ✓ | ✓ | ✓ |
| 05 팀 | ✓ | ✓ | ✓ |
| 06 로드맵 | ✓ | ✓ | ✓ |
| 07 비즈니스 모델 | ✓ | ✓ | - |
| 08 토크노믹스 | - | - | ✓ |
| 09 법적 고지 | 선택 | 선택 | ✓ |

## 리소스 요약

### 필수 섹션 (01-06)

| 리소스 | 목적 | 권장 분량 |
|--------|------|-----------|
| 01-overview.md | 프로젝트 소개, 문제 정의, 시장 기회 | 700-1,100 단어 |
| 02-solution.md | 솔루션 개요, 핵심 기능, 사용자 가치 | 850-1,300 단어 |
| 03-architecture.md | 시스템 구조, 기술 스택, 보안 | 1,050-1,500 단어 |
| 04-differentiation.md | 경쟁 분석, 핵심 차별점, 해자 | 1,000-1,500 단어 |
| 05-team.md | 핵심 팀원, 자문단, 파트너십 | 650-1,100 단어 |
| 06-roadmap.md | 개발 단계, 마일스톤, 장기 비전 | 650-1,050 단어 |

### 선택 섹션 (07-09)

| 리소스 | 적용 대상 | 권장 분량 |
|--------|-----------|-----------|
| 07-business-model.md | 일반, AI | 800-1,200 단어 |
| 08-tokenomics.md | 블록체인 | 850-1,250 단어 |
| 09-legal.md | 블록체인(필수), 기타(선택) | 700-1,050 단어 |

## 템플릿 요약

### whitepaper-general.md (일반 기술)
- **대상**: SaaS, B2B 솔루션, 플랫폼
- **섹션**: 요약 → 문제 → 해결책 → 아키텍처 → 차별화 → 팀 → 로드맵 → 비즈니스 모델 → 결론
- **특징**: 비즈니스 모델 중심

### whitepaper-ai.md (AI 프로젝트)
- **대상**: AI/ML 솔루션, LLM 서비스
- **섹션**: + 모델 아키텍처, 성능 벤치마크, AI 윤리
- **특징**: 모델 성능 및 윤리 강조

### whitepaper-blockchain.md (블록체인)
- **대상**: DeFi, NFT, Web3 프로젝트
- **섹션**: + 토크노믹스, 법적 고지
- **특징**: 토큰 경제 및 규제 준수 강조

## 사용 예시

### 전체 백서 작성 (AI 프로젝트)

```
1. Read templates/whitepaper-ai.md    # 전체 구조 확인
2. Read resources/01-overview.md      # 개요 작성 가이드
3. [개요 섹션 작성]
4. Read resources/02-solution.md      # 해결책 작성 가이드
5. [해결책 섹션 작성]
... (반복)
```

### 부분 수정 (로드맵 섹션만)

```
1. Read resources/06-roadmap.md       # 로드맵 가이드만 확인
2. [로드맵 섹션 수정]
```

## 분량 가이드

### 전체 백서 권장 분량

| 유형 | 단어 수 | 페이지 수 |
|------|---------|-----------|
| 일반 | 5,000-7,000 | 8-12 |
| AI | 6,000-8,000 | 10-14 |
| 블록체인 | 6,500-9,000 | 12-16 |

### 작성 원칙
- 문장 평균 길이: 14단어 이하
- 전문 용어: 첫 등장 시 설명
- 모든 수치: 출처 명시
- 시각 자료: 섹션당 1개 이상 권장

## 참고 자료

- [기술 백서 작성: 전문성 전달하기](https://www.jaenung.net/tree/19287)
- [How to Write a Technical White Paper (2025)](https://venngage.com/blog/how-to-write-technical-white-paper/)
- [Ultimate White Paper Format Guide 2025](https://blog.sagipl.com/white-paper-format/)
