# draw.io XML 명세

## 기본 구조

```xml
<mxfile host="app.diagrams.net" compressed="false">
  <diagram name="Page-1" id="unique-diagram-id">
    <mxGraphModel dx="1434" dy="836" grid="1" gridSize="10" guides="1" tooltips="1" connect="1" arrows="1" fold="1" page="1" pageScale="1" pageWidth="827" pageHeight="1169">
      <root>
        <mxCell id="0" />
        <mxCell id="1" parent="0" />
        <!-- 실제 도형과 연결선들 -->
      </root>
    </mxGraphModel>
  </diagram>
</mxfile>
```

## mxGraphModel 속성

| 속성 | 기본값 | 설명 |
|------|--------|------|
| `dx` | 1434 | 캔버스 X 오프셋 |
| `dy` | 836 | 캔버스 Y 오프셋 |
| `grid` | 1 | 그리드 표시 (0/1) |
| `gridSize` | 10 | 그리드 크기 |
| `guides` | 1 | 가이드라인 활성화 |
| `tooltips` | 1 | 툴팁 활성화 |
| `connect` | 1 | 연결 활성화 |
| `arrows` | 1 | 화살표 활성화 |
| `page` | 1 | 페이지 표시 |
| `pageWidth` | 827 | 페이지 너비 (A4) |
| `pageHeight` | 1169 | 페이지 높이 (A4) |
| `defaultFontFamily` | - | ⚠️ 기본 폰트 (PNG 출력 시 필요) |

## mxCell - 도형 (vertex)

```xml
<mxCell id="2" value="Label" style="rounded=0;whiteSpace=wrap;html=1;" vertex="1" parent="1">
  <mxGeometry x="100" y="100" width="120" height="60" as="geometry" />
</mxCell>
```

### 필수 속성
- `id`: 고유 식별자 (2부터 시작)
- `value`: 표시될 텍스트 (HTML 지원)
- `style`: 스타일 문자열
- `vertex="1"`: 도형임을 표시
- `parent="1"`: 기본 레이어

### mxGeometry 속성
- `x`, `y`: 위치 (왼쪽 상단 기준)
- `width`, `height`: 크기
- `as="geometry"`: 필수 속성

## mxCell - 연결선 (edge)

```xml
<mxCell id="5" value="" style="edgeStyle=orthogonalEdgeStyle;rounded=0;html=1;" edge="1" parent="1" source="2" target="3">
  <mxGeometry relative="1" as="geometry" />
</mxCell>
```

### 필수 속성
- `edge="1"`: 연결선임을 표시
- `source`: 시작 노드 ID
- `target`: 끝 노드 ID
- `relative="1"`: 상대 좌표 사용

## 주요 스타일

### 도형 스타일

| 스타일 | 값 | 설명 |
|--------|-----|------|
| `rounded` | 0/1 | 모서리 둥글게 |
| `whiteSpace` | wrap | 텍스트 줄바꿈 |
| `html` | 1 | HTML 렌더링 |
| `fillColor` | #색상 | 배경색 |
| `strokeColor` | #색상 | 테두리색 |
| `strokeWidth` | 숫자 | 테두리 두께 |
| `fontColor` | #색상 | 글자색 |
| `fontSize` | 숫자 | 글자 크기 (18px 권장) |
| `fontFamily` | 폰트명 | ⚠️ 폰트 (PNG 출력 필수) |
| `shape` | 도형명 | 특수 도형 |

### 도형 종류 (shape)

| shape 값 | 설명 |
|----------|------|
| `ellipse` | 타원/원 |
| `rhombus` | 마름모 (의사결정) |
| `parallelogram` | 평행사변형 |
| `cylinder3` | 실린더 (DB) |
| `cloud` | 구름 |
| `hexagon` | 육각형 |
| `actor` | 스틱맨 (액터) |

### 연결선 스타일

| 스타일 | 값 | 설명 |
|--------|-----|------|
| `edgeStyle` | orthogonalEdgeStyle | 직각 연결 |
| `edgeStyle` | elbowEdgeStyle | 꺾인 연결 |
| `edgeStyle` | entityRelationEdgeStyle | ER 스타일 |
| `curved` | 1 | 곡선 |
| `dashed` | 1 | 점선 |
| `endArrow` | classic/block/none | 끝 화살표 |
| `startArrow` | classic/block/none | 시작 화살표 |

## 색상 팔레트 (권장)

```
Primary:   #1976D2 (파랑)
Secondary: #388E3C (초록)
Warning:   #FFA726 (주황)
Error:     #D32F2F (빨강)
Neutral:   #757575 (회색)
Background: #FFFFFF, #F5F5F5, #E3F2FD
```

## 레이아웃 가이드

### 간격
- 노드 간 최소 간격: 40px
- 레이어 간 간격: 80-120px
- 페이지 여백: 40px
- 화살표와 라벨 간격: 20px 이상

### 크기 (권장)
- 기본 박스: 120x60
- 큰 박스: 160x80
- 원/타원: 80x80
- 마름모: 100x100
- 액터: 40x60

## 레이어 순서 (Z-order)

XML 기술 순서가 렌더링 Z-order를 결정합니다:
1. **edge(화살표) 먼저** → 최배면에 배치
2. **vertex(노드) 나중에** → 전면에 표시

```xml
<root>
  <mxCell id="0" />
  <mxCell id="1" parent="0" />
  <!-- edge를 먼저 기술 (최배면) -->
  <mxCell id="e1" edge="1" ... />
  <mxCell id="e2" edge="1" ... />
  <!-- vertex를 나중에 기술 (전면) -->
  <mxCell id="2" vertex="1" ... />
  <mxCell id="3" vertex="1" ... />
</root>
```

## 한글 텍스트 너비 가이드

한글은 영문보다 폭이 넓어 충분한 width 확보 필요:

| 문자 수 | 권장 width |
|--------|-----------|
| 2-3자  | 80px      |
| 4-5자  | 120px     |
| 6-8자  | 160px     |
| 8자+   | 200px+    |

**공식**: `width = 문자수 × 30~40px`

## 완전한 예제

```xml
<mxfile host="app.diagrams.net" compressed="false">
  <diagram name="Example" id="example-1">
    <!-- defaultFontFamily: 한글 폰트 지정 (예: Noto Sans KR) -->
    <mxGraphModel dx="1434" dy="836" grid="1" gridSize="10" guides="1" tooltips="1" connect="1" arrows="1" fold="1" page="1" pageScale="1" pageWidth="827" pageHeight="1169">
      <root>
        <mxCell id="0" />
        <mxCell id="1" parent="0" />
        <!-- edge를 먼저 기술 (최배면) -->
        <mxCell id="4" value="" style="edgeStyle=orthogonalEdgeStyle;rounded=0;html=1;endArrow=classic;" edge="1" parent="1" source="2" target="3">
          <mxGeometry relative="1" as="geometry" />
        </mxCell>
        <!-- vertex를 나중에 기술 (전면) -->
        <mxCell id="2" value="시작" style="ellipse;whiteSpace=wrap;html=1;fillColor=#E3F2FD;fontSize=18;" vertex="1" parent="1">
          <mxGeometry x="100" y="100" width="80" height="80" as="geometry" />
        </mxCell>
        <mxCell id="3" value="처리" style="rounded=0;whiteSpace=wrap;html=1;fillColor=#FFFFFF;fontSize=18;" vertex="1" parent="1">
          <mxGeometry x="80" y="220" width="120" height="60" as="geometry" />
        </mxCell>
      </root>
    </mxGraphModel>
  </diagram>
</mxfile>
```
