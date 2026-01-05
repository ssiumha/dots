# Diagram Slide

도식, 플로우차트, 아키텍처 다이어그램용. 박스와 화살표 조합.

## 레이아웃

```
┌─────────────────────────────────────────────────┐
│ [Header]                                        │
├─────────────────────────────────────────────────┤
│   Diagram Title                                 │
│                                                 │
│   ┌─────┐     ┌─────┐     ┌─────┐              │
│   │ Box │ ──▶ │ Box │ ──▶ │ Box │              │
│   └─────┘     └─────┘     └─────┘              │
│                                                 │
├─────────────────────────────────────────────────┤
│ [Footer]                                        │
└─────────────────────────────────────────────────┘
```

## 기본 도형 함수

```python
def add_box(slide, x, y, width, height, text, style,
            shape_type=MSO_SHAPE.ROUNDED_RECTANGLE, fill_color=None):
    """기본 박스 도형 추가"""
    shape = slide.shapes.add_shape(
        shape_type,
        Inches(x), Inches(y),
        Inches(width), Inches(height)
    )

    # 배경색
    if fill_color:
        shape.fill.solid()
        shape.fill.fore_color.rgb = fill_color
    else:
        shape.fill.solid()
        shape.fill.fore_color.rgb = style['primary']

    # 테두리
    shape.line.color.rgb = style.get('border', style['primary'])
    shape.line.width = Pt(1)

    # 텍스트
    tf = shape.text_frame
    tf.word_wrap = True
    tf.paragraphs[0].text = text
    tf.paragraphs[0].font.size = Pt(12)
    tf.paragraphs[0].font.color.rgb = style['white']
    tf.paragraphs[0].alignment = PP_ALIGN.CENTER

    # 수직 중앙 정렬
    shape.text_frame.anchor = MSO_ANCHOR.MIDDLE

    return shape


def add_arrow(slide, start_x, start_y, end_x, end_y, style, dashed=False):
    """화살표 (직선) 추가"""
    connector = slide.shapes.add_connector(
        MSO_CONNECTOR.STRAIGHT,
        Inches(start_x), Inches(start_y),
        Inches(end_x), Inches(end_y)
    )

    connector.line.color.rgb = style.get('arrow', style['text'])
    connector.line.width = Pt(2)

    if dashed:
        connector.line.dash_style = MSO_LINE_DASH_STYLE.DASH

    # 화살표 끝 (end arrow)
    # python-pptx는 직접 arrowhead 설정이 복잡함
    # 대안: 삼각형 도형으로 화살표 머리 추가

    return connector


def add_arrow_head(slide, x, y, direction, style):
    """화살표 머리 (삼각형)"""
    # direction: "right", "left", "up", "down"
    size = 0.15

    if direction == "right":
        shape_type = MSO_SHAPE.RIGHT_TRIANGLE
        rotation = 0
    elif direction == "left":
        shape_type = MSO_SHAPE.RIGHT_TRIANGLE
        rotation = 180
    elif direction == "down":
        shape_type = MSO_SHAPE.RIGHT_TRIANGLE
        rotation = 90
    else:  # up
        shape_type = MSO_SHAPE.RIGHT_TRIANGLE
        rotation = 270

    arrow = slide.shapes.add_shape(
        MSO_SHAPE.ISOSCELES_TRIANGLE,
        Inches(x), Inches(y),
        Inches(size), Inches(size)
    )
    arrow.rotation = rotation
    arrow.fill.solid()
    arrow.fill.fore_color.rgb = style.get('arrow', style['text'])
    arrow.line.fill.background()

    return arrow
```

## 플로우차트 슬라이드

```python
def add_flowchart_slide(prs, title, steps, style=None, header_func=None, page_num=1):
    """
    수평 플로우차트

    Args:
        steps: ["Step 1", "Step 2", "Step 3", ...]
    """
    blank_layout = prs.slide_layouts[6]
    slide = prs.slides.add_slide(blank_layout)

    if header_func:
        header_func(slide, title, style)
    add_footer(slide, page_num, style)

    # 제목
    title_box = slide.shapes.add_textbox(
        Inches(0.7), Inches(1.0),
        Inches(12), Inches(0.6)
    )
    tf = title_box.text_frame
    tf.paragraphs[0].text = title
    tf.paragraphs[0].font.size = Pt(28)
    tf.paragraphs[0].font.bold = True
    tf.paragraphs[0].font.color.rgb = style['title']

    # 박스 배치 계산
    num_steps = len(steps)
    box_width = 2.0
    box_height = 1.2
    gap = 0.8  # 박스 간 간격

    total_width = num_steps * box_width + (num_steps - 1) * gap
    start_x = (13.333 - total_width) / 2  # 중앙 정렬
    y = 3.5  # 수직 위치

    for i, step in enumerate(steps):
        x = start_x + i * (box_width + gap)

        # 박스 추가
        add_box(slide, x, y, box_width, box_height, step, style)

        # 화살표 (마지막 제외)
        if i < num_steps - 1:
            arrow_start_x = x + box_width + 0.1
            arrow_end_x = x + box_width + gap - 0.1
            arrow_y = y + box_height / 2

            add_arrow(slide, arrow_start_x, arrow_y, arrow_end_x, arrow_y, style)
            add_arrow_head(slide, arrow_end_x - 0.08, arrow_y - 0.075, "right", style)

    return slide
```

## 아키텍처 다이어그램

```python
def add_architecture_slide(prs, title, layers, style=None, header_func=None, page_num=1):
    """
    계층형 아키텍처 다이어그램

    Args:
        layers: [
            {"name": "Presentation", "items": ["Web", "Mobile", "API"]},
            {"name": "Business", "items": ["Service A", "Service B"]},
            {"name": "Data", "items": ["PostgreSQL", "Redis"]}
        ]
    """
    blank_layout = prs.slide_layouts[6]
    slide = prs.slides.add_slide(blank_layout)

    if header_func:
        header_func(slide, title, style)
    add_footer(slide, page_num, style)

    # 제목
    title_box = slide.shapes.add_textbox(
        Inches(0.7), Inches(1.0),
        Inches(12), Inches(0.6)
    )
    tf = title_box.text_frame
    tf.paragraphs[0].text = title
    tf.paragraphs[0].font.size = Pt(28)
    tf.paragraphs[0].font.bold = True
    tf.paragraphs[0].font.color.rgb = style['title']

    # 레이어 배치
    layer_height = 1.2
    layer_gap = 0.3
    start_y = 2.0

    colors = [style['primary'], style['secondary'], style.get('accent', style['primary'])]

    for layer_idx, layer in enumerate(layers):
        y = start_y + layer_idx * (layer_height + layer_gap)

        # 레이어 레이블 (좌측)
        label_box = slide.shapes.add_textbox(
            Inches(0.5), Inches(y + 0.4),
            Inches(1.5), Inches(0.5)
        )
        tf = label_box.text_frame
        tf.paragraphs[0].text = layer["name"]
        tf.paragraphs[0].font.size = Pt(11)
        tf.paragraphs[0].font.bold = True
        tf.paragraphs[0].font.color.rgb = style['text_light']

        # 아이템 박스들
        items = layer["items"]
        item_width = 2.2
        item_gap = 0.3
        total_items_width = len(items) * item_width + (len(items) - 1) * item_gap
        items_start_x = 2.5 + (10 - total_items_width) / 2

        fill_color = colors[layer_idx % len(colors)]

        for item_idx, item in enumerate(items):
            x = items_start_x + item_idx * (item_width + item_gap)
            add_box(slide, x, y, item_width, layer_height, item, style, fill_color=fill_color)

        # 레이어 간 화살표 (위에서 아래로)
        if layer_idx < len(layers) - 1:
            arrow_x = 6.5
            arrow_start_y = y + layer_height + 0.05
            arrow_end_y = y + layer_height + layer_gap - 0.05

            add_arrow(slide, arrow_x, arrow_start_y, arrow_x, arrow_end_y, style)
            add_arrow_head(slide, arrow_x - 0.075, arrow_end_y - 0.08, "down", style)

    return slide
```

## 커스텀 다이어그램

```python
def add_custom_diagram_slide(prs, title, boxes, arrows, style=None, header_func=None, page_num=1):
    """
    자유 배치 다이어그램

    Args:
        boxes: [
            {"id": "a", "x": 1, "y": 2, "w": 2, "h": 1, "text": "Box A"},
            {"id": "b", "x": 5, "y": 2, "w": 2, "h": 1, "text": "Box B"},
        ]
        arrows: [
            {"from": "a", "to": "b", "dashed": False},
        ]
    """
    blank_layout = prs.slide_layouts[6]
    slide = prs.slides.add_slide(blank_layout)

    if header_func:
        header_func(slide, title, style)
    add_footer(slide, page_num, style)

    # 제목
    title_box = slide.shapes.add_textbox(
        Inches(0.7), Inches(1.0),
        Inches(12), Inches(0.6)
    )
    tf = title_box.text_frame
    tf.paragraphs[0].text = title
    tf.paragraphs[0].font.size = Pt(28)
    tf.paragraphs[0].font.bold = True
    tf.paragraphs[0].font.color.rgb = style['title']

    # 박스 위치 저장 (화살표용)
    box_positions = {}

    for box in boxes:
        add_box(slide, box["x"], box["y"], box["w"], box["h"], box["text"], style,
                fill_color=box.get("color"))

        # 중심점 저장
        box_positions[box["id"]] = {
            "cx": box["x"] + box["w"] / 2,
            "cy": box["y"] + box["h"] / 2,
            "right": box["x"] + box["w"],
            "left": box["x"],
            "top": box["y"],
            "bottom": box["y"] + box["h"]
        }

    # 화살표 연결
    for arrow in arrows:
        from_box = box_positions[arrow["from"]]
        to_box = box_positions[arrow["to"]]

        # 수평 연결 (from 오른쪽 → to 왼쪽)
        if to_box["left"] > from_box["right"]:
            start_x, start_y = from_box["right"], from_box["cy"]
            end_x, end_y = to_box["left"], to_box["cy"]
            direction = "right"
        # 수직 연결 (from 아래 → to 위)
        else:
            start_x, start_y = from_box["cx"], from_box["bottom"]
            end_x, end_y = to_box["cx"], to_box["top"]
            direction = "down"

        add_arrow(slide, start_x, start_y, end_x, end_y, style, dashed=arrow.get("dashed", False))

        if direction == "right":
            add_arrow_head(slide, end_x - 0.08, end_y - 0.075, "right", style)
        else:
            add_arrow_head(slide, end_x - 0.075, end_y - 0.08, "down", style)

    return slide
```

## 사용 예시

```python
# 프로세스 플로우
add_flowchart_slide(
    prs,
    title="주문 처리 프로세스",
    steps=["주문 접수", "재고 확인", "결제 처리", "배송 준비", "배송 완료"],
    style=corporate_style,
    header_func=add_header,
    page_num=4
)

# 시스템 아키텍처
add_architecture_slide(
    prs,
    title="시스템 아키텍처",
    layers=[
        {"name": "Frontend", "items": ["React Web", "iOS App", "Android App"]},
        {"name": "Backend", "items": ["API Gateway", "Auth Service", "Order Service"]},
        {"name": "Data", "items": ["PostgreSQL", "Redis", "S3"]}
    ],
    style=corporate_style,
    header_func=add_header,
    page_num=5
)

# 커스텀 다이어그램
add_custom_diagram_slide(
    prs,
    title="데이터 흐름",
    boxes=[
        {"id": "client", "x": 1, "y": 3, "w": 2, "h": 1, "text": "Client"},
        {"id": "api", "x": 5, "y": 3, "w": 2, "h": 1, "text": "API Server"},
        {"id": "db", "x": 9, "y": 3, "w": 2, "h": 1, "text": "Database"},
    ],
    arrows=[
        {"from": "client", "to": "api"},
        {"from": "api", "to": "db"},
    ],
    style=corporate_style,
    header_func=add_header,
    page_num=6
)
```

## 도형 타입 참조

| 용도 | MSO_SHAPE |
|------|-----------|
| 기본 박스 | ROUNDED_RECTANGLE |
| 프로세스 | RECTANGLE |
| 결정/분기 | DIAMOND |
| 시작/끝 | OVAL |
| 데이터 | PARALLELOGRAM |
| 문서 | WAVE |
| 데이터베이스 | CAN |

---

## 자동 레이아웃 (Auto Layout)

복잡한 다이어그램의 겹침 방지 및 깔끔한 연결을 위해 레이아웃 엔진 사용.

**참조**: `resources/04-layout-engine.md`

### 자동 배치 다이어그램

```python
from layout_engine import DiagramGrid, ArrowRouter, ElementPorts

def add_auto_layout_diagram(prs, title, elements, connections, style=None, header_func=None, page_num=1):
    """
    자동 레이아웃 다이어그램

    Args:
        elements: ["EC2", "RDS", "S3", "Lambda"]  # 이름만 지정
        connections: [
            {"from": "EC2", "to": "RDS"},
            {"from": "EC2", "to": "S3"},
            {"from": "Lambda", "to": "RDS"},
        ]
    """
    blank_layout = prs.slide_layouts[6]
    slide = prs.slides.add_slide(blank_layout)

    if header_func:
        header_func(slide, title, style)
    add_footer(slide, page_num, style)

    # 제목
    title_box = slide.shapes.add_textbox(
        Inches(0.7), Inches(1.0),
        Inches(12), Inches(0.6)
    )
    tf = title_box.text_frame
    tf.paragraphs[0].text = title
    tf.paragraphs[0].font.size = Pt(28)
    tf.paragraphs[0].font.bold = True
    tf.paragraphs[0].font.color.rgb = style['title']

    # 그리드 초기화
    grid = DiagramGrid(cols=12, rows=6)
    router = ArrowRouter(grid)

    # 요소 자동 배치
    element_ids = {}
    for i, elem in enumerate(elements):
        elem_id = elem.lower().replace(" ", "_")
        pos = grid.find_free_position(2, 1)  # 2x1 셀 크기
        if pos:
            grid.place(elem_id, pos[0], pos[1], 2, 1)
            element_ids[elem] = elem_id

    # 포트 시스템으로 다중 연결 관리
    ports = {}
    for elem_id, data in grid.elements.items():
        x, y = grid.to_inches(data["row"], data["col"])
        ports[elem_id] = ElementPorts({
            "x": x, "y": y,
            "width": grid.cell_width * data["w"],
            "height": grid.cell_height * data["h"]
        })

    # 연결 카운트 (포트 분배용)
    conn_count = {}
    for conn in connections:
        from_id = element_ids[conn["from"]]
        to_id = element_ids[conn["to"]]
        conn_count.setdefault(from_id, {"right": 0, "left": 0, "top": 0, "bottom": 0})
        conn_count.setdefault(to_id, {"right": 0, "left": 0, "top": 0, "bottom": 0})

    # 박스 그리기
    for elem, elem_id in element_ids.items():
        data = grid.elements[elem_id]
        x, y = grid.to_inches(data["row"], data["col"])
        w = grid.cell_width * data["w"]
        h = grid.cell_height * data["h"]
        add_box(slide, x, y, w, h, elem, style)

    # 화살표 라우팅 (직교 경로)
    for conn in connections:
        from_id = element_ids[conn["from"]]
        to_id = element_ids[conn["to"]]
        waypoints = conn.get("waypoints")  # 수동 경유점

        path = router.route(from_id, to_id, waypoints=waypoints)
        draw_orthogonal_path(slide, path, style, dashed=conn.get("dashed", False))

    return slide


def draw_orthogonal_path(slide, path, style, dashed=False):
    """직교 경로 그리기 (꺾인 선)"""
    for i in range(len(path) - 1):
        x1, y1 = path[i]
        x2, y2 = path[i + 1]
        add_arrow(slide, x1, y1, x2, y2, style, dashed=dashed)

    # 마지막 구간에 화살표 머리
    if len(path) >= 2:
        x1, y1 = path[-2]
        x2, y2 = path[-1]
        if abs(x2 - x1) > abs(y2 - y1):  # 수평
            direction = "right" if x2 > x1 else "left"
            add_arrow_head(slide, x2 - 0.08, y2 - 0.075, direction, style)
        else:  # 수직
            direction = "down" if y2 > y1 else "up"
            add_arrow_head(slide, x2 - 0.075, y2 - 0.08, direction, style)
```

### ASCII 기반 다이어그램

직관적인 ASCII art로 레이아웃 정의 후 자동 변환.

```python
from layout_engine import AsciiLayoutParser

def add_ascii_diagram(prs, title, ascii_layout, labels, connections, style=None, header_func=None, page_num=1):
    """
    ASCII 레이아웃 기반 다이어그램

    Args:
        ascii_layout: '''
            +---+         +---+
            | A |-------->| B |
            +---+         +---+
              |
              v
            +---+
            | C |
            +---+
        '''
        labels: {"A": "Client", "B": "Server", "C": "Database"}
        connections: [{"from": "A", "to": "B"}, {"from": "A", "to": "C"}]
    """
    blank_layout = prs.slide_layouts[6]
    slide = prs.slides.add_slide(blank_layout)

    if header_func:
        header_func(slide, title, style)
    add_footer(slide, page_num, style)

    # 제목
    title_box = slide.shapes.add_textbox(
        Inches(0.7), Inches(1.0),
        Inches(12), Inches(0.6)
    )
    tf = title_box.text_frame
    tf.paragraphs[0].text = title
    tf.paragraphs[0].font.size = Pt(28)
    tf.paragraphs[0].font.bold = True
    tf.paragraphs[0].font.color.rgb = style['title']

    # ASCII 파싱
    parser = AsciiLayoutParser(cols=12, rows=6)
    parsed = parser.parse(ascii_layout)

    # 그리드 초기화 및 배치
    grid = DiagramGrid(cols=12, rows=6)
    router = ArrowRouter(grid)

    for box_id, pos in parsed["boxes"].items():
        grid.place(box_id, pos["row"], pos["col"], pos.get("w", 2), pos.get("h", 1))

    # 박스 그리기
    for box_id, data in grid.elements.items():
        x, y = grid.to_inches(data["row"], data["col"])
        w = grid.cell_width * data["w"]
        h = grid.cell_height * data["h"]
        label = labels.get(box_id, box_id)
        add_box(slide, x, y, w, h, label, style)

    # 화살표
    for conn in connections:
        path = router.route(conn["from"], conn["to"])
        draw_orthogonal_path(slide, path, style)

    return slide
```

### 사용 예시 - 자동 레이아웃

```python
# Before: 수동 좌표 지정 (겹침 위험)
add_custom_diagram_slide(prs, "Architecture", boxes=[
    {"id": "a", "x": 1, "y": 3, "w": 2, "h": 1, "text": "A"},
    {"id": "b", "x": 5, "y": 3, "w": 2, "h": 1, "text": "B"},  # 겹칠 수 있음
], ...)

# After: 자동 배치 (겹침 방지)
add_auto_layout_diagram(prs, "Architecture",
    elements=["API Gateway", "Auth Service", "User Service", "DB"],
    connections=[
        {"from": "API Gateway", "to": "Auth Service"},
        {"from": "API Gateway", "to": "User Service"},
        {"from": "User Service", "to": "DB"},
    ],
    style=corporate_style,
    header_func=add_header
)
```

### 사용 예시 - ASCII 레이아웃

```python
# 직관적인 ASCII로 구조 정의
ascii_layout = """
+-----+     +-----+     +-----+
|  A  |---->|  B  |---->|  C  |
+-----+     +-----+     +-----+
              |
              v
            +-----+
            |  D  |
            +-----+
"""

add_ascii_diagram(prs, "데이터 흐름",
    ascii_layout=ascii_layout,
    labels={"A": "Client", "B": "API", "C": "Cache", "D": "Database"},
    connections=[
        {"from": "A", "to": "B"},
        {"from": "B", "to": "C"},
        {"from": "B", "to": "D"},
    ],
    style=corporate_style
)
```

### 수동 Waypoint 지정

복잡한 경로가 필요할 때 경유점을 직접 지정:

```python
connections=[
    {
        "from": "A",
        "to": "C",
        "waypoints": [(5.0, 3.0), (5.0, 5.0)]  # 중간 꺾임점
    },
]
```
