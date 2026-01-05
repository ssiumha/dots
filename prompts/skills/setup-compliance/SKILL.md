---
name: setup-compliance
description: 프로젝트에 금융 규제 준수 문서를 생성합니다. "규제", "FSS", "ISMS", "PCI DSS", "compliance 설정" 요청 시 사용하세요.
---

# Setup Compliance

프로젝트에 금융 규제 준수 문서를 **동적으로** 생성합니다.

**핵심 철학**:
- 실행 시점에 **WebSearch**로 최신 규제 검색
- 검색 결과 기반으로 **동적 생성**
- 고정 템플릿 복사 아님

## Instructions

### 워크플로우 1: 신규 규제 준수 설정

#### 1. 규제 선택

AskUserQuestion (multiSelect: true):
- FSS (금융감독원)
- ISMS-P (정보보호 관리체계)
- PCI DSS (카드 데이터 보안)
- K-ISMS (클라우드 보안)
- 기타 (직접 입력)

#### 2. 최신 규제 검색

선택한 규제별로 WebSearch 실행:

```
resources/01-regulation-keywords.md 참조하여 검색
```

**검색 예시**:
- FSS: "전자금융거래법 2025 감사로그 요구사항"
- PCI DSS: "PCI DSS 4.0 requirements 2025"

#### 3. 검색 결과 분석

검색 결과에서 추출:
- 핵심 요구사항 목록
- 보관 기간
- 벌칙/제재
- 최신 변경 사항

#### 4. 문서 생성

```bash
mkdir -p docs/compliance
```

**생성할 문서**:

1. `README.md` - 허브 문서
   - 적용 규제 목록
   - 준수율 대시보드 (초기 0%)
   - 검토 일정

2. `{규제}-checklist.md` - 규제별 체크리스트
   - 법적 근거 (검색 결과에서)
   - 요구사항 테이블 (❌ 상태로 초기화)
   - 구현 위치 컬럼 (빈칸)

3. `audit-trail-spec.md` - 감사 로그 명세 (공통)
   - 필수 필드
   - 이벤트 유형

4. `retention-policy.md` - 보관 정책 (공통)
   - 규제별 보관 기간

#### 5. 사용자 안내

- 생성된 파일 목록
- 다음 단계 (체크리스트 검토)
- 검색 출처 링크

### 워크플로우 2: 규제 추가

1. 기존 docs/compliance/ 확인
2. 추가할 규제 선택
3. WebSearch로 해당 규제 검색
4. 새 체크리스트 생성
5. README.md 업데이트

### 워크플로우 3: 체크리스트 업데이트

1. 현재 체크리스트 읽기
2. 완료 항목 ✅ 표시
3. 구현 위치 추가
4. 준수율 갱신

## 문서 구조

```
{project}/docs/compliance/
├── README.md              # 허브
├── {규제}-checklist.md    # 규제별 (동적 생성)
├── audit-trail-spec.md    # 감사 로그
└── retention-policy.md    # 보관 정책
```

## 체크리스트 형식

```markdown
# {규제} 체크리스트

> {검색에서 얻은 규제 설명}

## 법적 근거

> {검색에서 얻은 법 조문}

## 요구사항

| ID | 요구사항 | 상태 | 구현 위치 |
|----|---------|------|----------|
| 01 | {검색 결과} | ❌ | |

## 출처

- {WebSearch 출처 URL}
```

## 중요 원칙

1. **최신 정보**: 항상 WebSearch로 최신 규제 확인
2. **출처 명시**: 검색 결과 URL 포함
3. **동적 생성**: 고정 템플릿 복사 아님
4. **프로젝트 맞춤**: 선택한 규제만 생성

## Examples

### 예시 1: FSS 규제 설정

```
User: "이 프로젝트에 FSS 규제 준수 설정해줘"

→ AskUserQuestion: 규제 선택 (FSS 선택됨)
→ WebSearch "전자금융거래법 2025 감사로그 요구사항"
→ WebSearch "FSS 로그 보관 기간"
→ 검색 결과에서 요구사항 추출
→ docs/compliance/ 생성
  - README.md (허브)
  - fss-checklist.md (동적 생성)
  - audit-trail-spec.md
  - retention-policy.md
→ 출처 URL 포함하여 완료
```

### 예시 2: 복수 규제 설정

```
User: "FSS랑 PCI DSS 둘 다 적용해야 해"

→ AskUserQuestion: 규제 선택 (FSS, PCI DSS 선택)
→ WebSearch (FSS 관련 2-3개)
→ WebSearch (PCI DSS 관련 2-3개)
→ docs/compliance/ 생성
  - fss-checklist.md
  - pci-dss-checklist.md
  - README.md (두 규제 모두 포함)
```

## Technical Details

규제별 검색 키워드는 `resources/01-regulation-keywords.md` 참조
