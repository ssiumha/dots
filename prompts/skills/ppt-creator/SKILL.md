---
name: ppt-creator
description: Creates PowerPoint presentations via python-pptx. Use when building slides for presentations, proposals, or planning documents.
---

# PPT Creator

python-pptx 기반 프레젠테이션 생성. 슬라이드별 모듈화된 코드 제공.

## 구조

```
[Header Template] + [Slide Types] + [Style Preset]
      ↓                  ↓              ↓
   고정 상단           콘텐츠 영역      색상/폰트
```

## 워크플로우

### WF1: 새 프레젠테이션 생성

1. **요구사항 파악**: 용도, 슬라이드 수, 스타일 확인
2. **구성 요소 선택**:
   - 헤더 템플릿 (minimal / nav / progress)
   - 스타일 프리셋 (corporate / startup / minimal)
3. **아웃라인 제시**: 슬라이드 구성안 → 사용자 확인
4. **코드 생성**: 슬라이드별 코드 조합
5. **실행 안내**: 사용자가 코드 실행 → .pptx 생성

### WF2: 슬라이드 추가/수정

1. **기존 코드 확인**: 어떤 구성인지 파악
2. **슬라이드 타입 선택**: 필요한 타입 코드 제공
3. **삽입 위치 안내**: 코드 어디에 추가할지 설명

## 헤더 템플릿

| 템플릿 | 용도 | 리소스 |
|--------|------|--------|
| minimal | 로고+제목만, 깔끔한 제안서 | `01-headers/minimal.md` |
| nav | 상단 탭 네비, 다섹션 문서 | `01-headers/nav.md` |
| progress | 진행률 바, 단계별 설명 | `01-headers/progress.md` |

## 슬라이드 타입

| 타입 | 용도 | 리소스 |
|------|------|--------|
| title | 표지 | `02-slides/title.md` |
| section | 섹션 구분 | `02-slides/section.md` |
| content | 제목+불릿 | `02-slides/content.md` |
| two-column | 2단 비교 | `02-slides/two-column.md` |
| image-text | 이미지+설명 | `02-slides/image-text.md` |
| diagram | 플로우/아키텍처 | `02-slides/diagram.md` |
| aws-diagram | AWS 아키텍처 | `02-slides/aws-diagram.md` |
| chart | 차트 중심 | `02-slides/chart.md` |
| table | 테이블 | `02-slides/table.md` |
| closing | 마무리/연락처 | `02-slides/closing.md` |

## 스타일 프리셋

| 프리셋 | 특징 | 리소스 |
|--------|------|--------|
| corporate | 파랑/회색, 안정감 | `03-styles/corporate.md` |
| startup | 그라디언트, 활기 | `03-styles/startup.md` |
| minimal | 흑백, 깔끔 | `03-styles/minimal.md` |

## 코드 구조

```python
# 전체 import는 resources/00-base-imports.md 참조
from pptx import Presentation
from pptx.util import Inches, Pt
from pptx.dml.color import RGBColor
from pptx.enum.text import PP_ALIGN
from pptx.enum.shapes import MSO_SHAPE  # 도형용

# === Style Constants ===
# resources/03-styles/{preset}.md 에서 로드

# === Header Setup ===
# resources/01-headers/{template}.md 에서 로드

# === Slides ===
# resources/02-slides/{type}.md 에서 로드

prs = Presentation()
prs.slide_width = Inches(13.333)
prs.slide_height = Inches(7.5)

# 슬라이드 추가...
add_title_slide(prs, "제목", "부제목")
add_content_slide(prs, "섹션", ["항목1", "항목2"])

prs.save("output.pptx")
```

## 중요 원칙

1. **헤더 일관성**: 콘텐츠 슬라이드에 동일 헤더 적용
2. **콘텐츠 영역 분리**: 헤더 아래 영역만 슬라이드별 변경
3. **스타일 상수화**: 색상/폰트는 상단에 상수로 정의
4. **함수화**: 슬라이드 타입별 함수로 분리

**참고**: `title`, `section`, `closing`은 전체 화면 슬라이드로 `header_func` 파라미터 없음.

## Examples

### 투자 제안서 (Startup + Progress Header)

```
User: "투자 유치용 피치덱 만들어줘"

→ 구성:
  - Header: progress (10단계 진행률)
  - Style: startup (그라디언트, 활기찬)
  - Slides:
    1. title: 회사명, 슬로건
    2. content: 문제 정의
    3. content: 솔루션
    4. two-column: 경쟁사 비교
    5. chart: 시장 규모
    6. image-text: 제품 스크린샷
    7. content: 비즈니스 모델
    8. table: 재무 계획
    9. content: 팀 소개
    10. closing: 투자 요청
```

### 프로젝트 기획서 (Corporate + Minimal Header)

```
User: "신규 서비스 기획서"

→ 구성:
  - Header: minimal (로고+제목)
  - Style: corporate (파랑/회색)
  - Slides:
    1. title: 프로젝트명
    2. section: 배경
    3. content: 현황 분석
    4. content: 목표
    5. two-column: AS-IS / TO-BE
    6. table: 일정
    7. chart: 예산
    8. closing: 기대효과
```

## 다이어그램 자동 레이아웃

복잡한 아키텍처 다이어그램의 겹침 방지를 위한 레이아웃 엔진 제공.

**참조**: `resources/04-layout-engine.md`

| 기능 | 설명 |
|------|------|
| DiagramGrid | 12x6 그리드 기반 자동 배치, 충돌 감지 |
| ElementPorts | 다중 연결 포트 분배 (ALB → 여러 EC2) |
| ArrowRouter | 직교 화살표 라우팅, 장애물 회피 |
| AsciiLayoutParser | ASCII art → 그리드 좌표 변환 |

### 사용 시기

- AWS 아키텍처 다이어그램 (3+ 서비스)
- 복잡한 연결 관계 (한 컴포넌트에서 다수 연결)
- 수동 좌표 지정 시 겹침 발생

### 사용 방법

```python
# 자동 레이아웃 활성화
add_auto_layout_diagram(prs, "Architecture",
    elements=["API Gateway", "Auth", "User Service", "DB"],
    connections=[
        {"from": "API Gateway", "to": "Auth"},
        {"from": "API Gateway", "to": "User Service"},
    ],
    style=corporate_style
)

# ASCII 기반 레이아웃
ascii_layout = """
+-----+     +-----+
|  A  |---->|  B  |
+-----+     +-----+
"""
add_ascii_diagram(prs, "Flow", ascii_layout, labels={...})
```

## Anti-Patterns

- 헤더 없이 슬라이드 생성 (일관성 깨짐)
- 하드코딩된 색상값 (스타일 상수 사용)
- 슬라이드마다 다른 폰트 크기 (상수 사용)
- 복잡한 다이어그램에 수동 좌표 사용 (자동 레이아웃 사용)

## Technical Details

- `resources/00-base-imports.md`: 공통 import 문
- `resources/01-headers/`: 헤더 템플릿 코드
- `resources/02-slides/`: 슬라이드 타입 코드
- `resources/03-styles/`: 스타일 프리셋 (색상, 폰트)
- `resources/04-layout-engine.md`: 다이어그램 자동 레이아웃 엔진
