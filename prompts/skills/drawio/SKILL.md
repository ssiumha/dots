---
name: drawio
description: Generates draw.io diagrams for visualization. Use when creating architecture, sequence, flowchart, or AWS infrastructure diagrams.
---

# draw.io 다이어그램 생성

draw.io 호환 XML을 직접 생성하여 `.drawio` 파일로 저장합니다.

**핵심 원칙**:
- 비압축 XML 형식 (`compressed="false"`)으로 생성하여 가독성 확보
- 다이어그램 유형별 최적화된 스타일 적용
- 토큰 효율을 위해 필요한 리소스만 선택적 로드

## Instructions

### 워크플로우: 요청 분석 및 다이어그램 생성

#### 1. 키워드 매칭

사용자 요청에서 다이어그램 유형을 파악하고 해당 리소스를 로드합니다.

**Architecture** (`resources/01-architecture.md`)
- "아키텍처", "architecture", "시스템 구조", "AWS", "클라우드", "컴포넌트", "인프라"

**Flowchart** (`resources/02-flowchart.md`)
- "플로우차트", "flowchart", "프로세스", "흐름도", "의사결정", "워크플로우"

**Sequence** (`resources/03-sequence.md`)
- "시퀀스", "sequence", "API 호출", "메시지 흐름", "상호작용"

#### 2. 리소스 로딩 전략

**단일 유형**:
```
User: "로그인 프로세스 플로우차트 그려줘"
→ Read resources/02-flowchart.md
→ 플로우차트 패턴 적용
```

**복합 요청**:
```
User: "시스템 아키텍처와 API 호출 흐름 다이어그램"
→ Read resources/01-architecture.md
→ Read resources/03-sequence.md
→ 두 개의 다이어그램 생성 또는 통합
```

**불명확한 요청**:
```
User: "다이어그램 그려줘"
→ AskUserQuestion으로 유형 확인
```

#### 3. XML 생성

1. **리소스 Read**: 해당 유형의 XML 패턴 확인
2. **구조 설계**: 요청에 맞는 노드/엣지 구성
3. **XML 작성**: 비압축 mxfile 형식으로 생성
4. **파일 저장**: `.drawio` 확장자로 Write

#### 4. 파일 저장 위치

사용자에게 저장 위치를 확인:
- 프로젝트 내 `docs/diagrams/` (권장)
- 사용자 지정 경로

#### 5. 완료 안내

```
다이어그램을 생성했습니다: {path}

draw.io에서 열기:
- 데스크톱 앱: File > Open
- 웹: https://app.diagrams.net > File > Open from > Device
```

## 중요 원칙

1. **비압축 XML**: 항상 `compressed="false"` 설정으로 가독성 확보
2. ⚠️ **고유 ID**: mxCell id는 고유해야 함 (2부터 시작, 순차 증가) - 중복 시 오류
3. **parent 계층**: 모든 셀은 parent="1" (기본 레이어) 지정
4. **스타일 일관성**: 같은 유형의 노드는 동일한 스타일 적용
5. **적절한 간격**: 노드 간 최소 40px 간격 유지
6. ⚠️ **연결선 명확성**: edge의 source/target ID가 존재하는지 확인 필수

## PNG 출력 품질 가이드

### 폰트 설정
PNG 출력 시 폰트가 올바르게 렌더링되려면 **두 가지 설정**이 모두 필요:

1. mxGraphModel에 `defaultFontFamily="폰트명"` 설정
2. 각 텍스트 요소의 style에 `fontFamily=폰트명;` 추가

**권장 폰트 (한글 지원):**
- Noto Sans KR (Google 무료 폰트)
- Malgun Gothic (Windows)
- Apple SD Gothic Neo (macOS)

### 레이어 순서 (Z-order)
XML 기술 순서가 렌더링 순서를 결정:
- **edge(화살표)를 먼저** 기술 → 최배면에 배치
- 그 다음 vertex(노드) 기술 → 화살표 위에 표시

### 텍스트 크기
- 폰트 사이즈: 18px 권장 (표준 12px의 1.5배)
- 한글 텍스트 너비: 1문자당 30-40px 확보
- 화살표와 라벨 간 최소 20px 거리

## 검증 (필요 시)

PNG 출력 품질이 중요한 경우:
```bash
python scripts/drawio-tools.py validate {file}.drawio  # XML 검증
python scripts/drawio-tools.py export {file}.drawio    # PNG 출력
```

출력된 PNG를 Read하여 폰트/레이아웃 확인 가능.

## 안티패턴

### ❌ ID 중복
mxCell id가 중복되면 draw.io에서 오류 발생. 항상 고유 ID 사용.

### ❌ 압축 XML 생성
`compressed="true"`는 사람이 읽을 수 없음. 디버깅 불가.

## Examples

### 아키텍처 다이어그램
User: "3-tier 웹 아키텍처 그려줘"
→ 키워드 "아키텍처" 매칭
→ Read resources/01-architecture.md
→ Frontend, Backend, Database 레이어 구성
→ `docs/diagrams/architecture.drawio` 저장

### 플로우차트
User: "주문 처리 프로세스 플로우차트"
→ 키워드 "프로세스", "플로우차트" 매칭
→ Read resources/02-flowchart.md
→ 시작 → 주문확인 → 재고확인 → 결제 → 배송 구성
→ `docs/diagrams/order-flow.drawio` 저장

### 시퀀스 다이어그램
User: "로그인 API 시퀀스 다이어그램"
→ 키워드 "API", "시퀀스" 매칭
→ Read resources/03-sequence.md
→ Client → Server → DB 호출 흐름 구성
→ `docs/diagrams/login-sequence.drawio` 저장

### AWS 아키텍처
User: "AWS 3-tier 웹 아키텍처 그려줘"
→ 키워드 "AWS", "아키텍처" 매칭
→ Read resources/01-architecture.md (AWS 섹션)
→ Region > VPC > Subnet 그룹 구성
→ ALB, EC2, RDS 서비스 배치
→ `docs/diagrams/aws-architecture.drawio` 저장

## Technical Details

### XML 검증 도구

생성된 다이어그램 검증:
```bash
python scripts/drawio-tools.py validate {file}.drawio   # XML 구조 검증
python scripts/drawio-tools.py info {file}.drawio       # 다이어그램 정보
python scripts/drawio-tools.py list {file}.drawio       # 셀 목록
python scripts/drawio-tools.py styles {file}.drawio     # 스타일 분석
python scripts/drawio-tools.py aws {file}.drawio        # AWS 색상 검증
```

### 참조 문서
- `REFERENCE.md`: mxGraph XML 구조 명세
- `resources/01-architecture.md`: 아키텍처 다이어그램 패턴
- `resources/02-flowchart.md`: 플로우차트 패턴
- `resources/03-sequence.md`: 시퀀스 다이어그램 패턴
