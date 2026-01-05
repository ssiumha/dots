# AWS Architecture Diagram

AWS 아키텍처 도식용. 공식 아이콘 + 박스 레이아웃.

## AWS 아이콘 리소스

### 공식 다운로드
- **AWS Architecture Icons**: https://aws.amazon.com/architecture/icons/
- 형식: SVG, PNG (48x48, 64x64)
- 라이선스: AWS 문서/프레젠테이션에 무료 사용 가능

### 권장 구조
```
assets/aws-icons/
├── compute/
│   ├── ec2.png
│   ├── lambda.png
│   └── ecs.png
├── database/
│   ├── rds.png
│   ├── dynamodb.png
│   └── elasticache.png
├── storage/
│   ├── s3.png
│   └── efs.png
├── networking/
│   ├── vpc.png
│   ├── elb.png
│   ├── route53.png
│   └── cloudfront.png
└── general/
    ├── user.png
    ├── internet.png
    └── region.png
```

## AWS 스타일 색상

```python
aws_style = {
    # AWS 공식 색상
    "primary": RGBColor(35, 47, 62),       # #232F3E (AWS Navy)
    "secondary": RGBColor(255, 153, 0),    # #FF9900 (AWS Orange)
    "accent": RGBColor(0, 164, 239),       # #00A4EF (AWS Blue)

    # 서비스별 색상
    "compute": RGBColor(237, 127, 37),     # #ED7F25 (Orange)
    "database": RGBColor(60, 99, 186),     # #3C63BA (Blue)
    "storage": RGBColor(86, 158, 49),      # #569E31 (Green)
    "networking": RGBColor(134, 79, 162),  # #864FA2 (Purple)
    "security": RGBColor(221, 68, 68),     # #DD4444 (Red)

    # 텍스트/배경
    "title": RGBColor(35, 47, 62),
    "text": RGBColor(35, 47, 62),
    "text_light": RGBColor(102, 102, 102),
    "white": RGBColor(255, 255, 255),
    "background": RGBColor(255, 255, 255),

    # 다이어그램용
    "arrow": RGBColor(102, 102, 102),
    "border": RGBColor(200, 200, 200),
    "vpc_bg": RGBColor(237, 244, 252),     # 연한 파랑 (VPC 배경)
    "subnet_public": RGBColor(212, 237, 218),   # 연한 초록
    "subnet_private": RGBColor(230, 230, 250),  # 연한 보라
}
```

## AWS 서비스 박스

```python
# 필수 import: import os (00-base-imports.md 참조)

def add_aws_service(slide, x, y, icon_path, label, style, category="compute"):
    """
    AWS 서비스 아이콘 + 라벨

    Args:
        icon_path: AWS 아이콘 이미지 경로
        label: 서비스명 (예: "EC2", "RDS")
        category: compute/database/storage/networking/security
    """
    icon_size = 0.6
    box_width = 1.2
    box_height = 1.0

    # 배경 박스 (카테고리 색상)
    bg = slide.shapes.add_shape(
        MSO_SHAPE.ROUNDED_RECTANGLE,
        Inches(x), Inches(y),
        Inches(box_width), Inches(box_height)
    )
    bg.fill.solid()
    bg.fill.fore_color.rgb = style['white']
    bg.line.color.rgb = style.get(category, style['border'])
    bg.line.width = Pt(2)

    # 아이콘
    if icon_path and os.path.exists(icon_path):
        icon_x = x + (box_width - icon_size) / 2
        icon_y = y + 0.1
        slide.shapes.add_picture(
            icon_path,
            Inches(icon_x), Inches(icon_y),
            width=Inches(icon_size)
        )

    # 라벨
    label_box = slide.shapes.add_textbox(
        Inches(x), Inches(y + 0.75),
        Inches(box_width), Inches(0.25)
    )
    tf = label_box.text_frame
    tf.paragraphs[0].text = label
    tf.paragraphs[0].font.size = Pt(9)
    tf.paragraphs[0].font.bold = True
    tf.paragraphs[0].font.color.rgb = style['text']
    tf.paragraphs[0].alignment = PP_ALIGN.CENTER

    return bg


def add_vpc_box(slide, x, y, width, height, label, style):
    """VPC/Region 컨테이너 박스"""
    vpc = slide.shapes.add_shape(
        MSO_SHAPE.RECTANGLE,
        Inches(x), Inches(y),
        Inches(width), Inches(height)
    )
    vpc.fill.solid()
    vpc.fill.fore_color.rgb = style['vpc_bg']
    vpc.line.color.rgb = style['border']
    vpc.line.width = Pt(1)
    vpc.line.dash_style = MSO_LINE_DASH_STYLE.DASH

    # VPC 라벨 (좌상단)
    label_box = slide.shapes.add_textbox(
        Inches(x + 0.1), Inches(y + 0.05),
        Inches(2), Inches(0.3)
    )
    tf = label_box.text_frame
    tf.paragraphs[0].text = label
    tf.paragraphs[0].font.size = Pt(10)
    tf.paragraphs[0].font.bold = True
    tf.paragraphs[0].font.color.rgb = style['text_light']

    return vpc


def add_subnet_box(slide, x, y, width, height, label, is_public, style):
    """Subnet 박스 (Public/Private)"""
    subnet = slide.shapes.add_shape(
        MSO_SHAPE.RECTANGLE,
        Inches(x), Inches(y),
        Inches(width), Inches(height)
    )
    subnet.fill.solid()
    subnet.fill.fore_color.rgb = (
        style['subnet_public'] if is_public else style['subnet_private']
    )
    subnet.line.color.rgb = style['border']
    subnet.line.width = Pt(1)

    # Subnet 라벨
    label_box = slide.shapes.add_textbox(
        Inches(x + 0.1), Inches(y + 0.05),
        Inches(width - 0.2), Inches(0.25)
    )
    tf = label_box.text_frame
    tf.paragraphs[0].text = label
    tf.paragraphs[0].font.size = Pt(9)
    tf.paragraphs[0].font.color.rgb = style['text_light']

    return subnet
```

## AWS 아키텍처 슬라이드

```python
def add_aws_architecture_slide(prs, title, config, style=None, header_func=None, page_num=1):
    """
    AWS 아키텍처 다이어그램

    Args:
        config: {
            "vpc": {"x": 2, "y": 1.5, "width": 10, "height": 5},
            "subnets": [
                {"x": 2.2, "y": 2, "width": 4.5, "height": 4, "label": "Public Subnet", "public": True},
                {"x": 7, "y": 2, "width": 4.5, "height": 4, "label": "Private Subnet", "public": False},
            ],
            "services": [
                {"x": 3, "y": 2.5, "icon": "aws-icons/networking/elb.png", "label": "ALB", "cat": "networking"},
                {"x": 5, "y": 2.5, "icon": "aws-icons/compute/ec2.png", "label": "EC2", "cat": "compute"},
                {"x": 8, "y": 2.5, "icon": "aws-icons/database/rds.png", "label": "RDS", "cat": "database"},
            ],
            "arrows": [
                {"from": [3.6, 3], "to": [5, 3]},
                {"from": [5.6, 3], "to": [8, 3]},
            ]
        }
    """
    blank_layout = prs.slide_layouts[6]
    slide = prs.slides.add_slide(blank_layout)

    if header_func:
        header_func(slide, title, style)
    add_footer(slide, page_num, style)

    # 제목
    title_box = slide.shapes.add_textbox(
        Inches(0.7), Inches(1.0),
        Inches(12), Inches(0.5)
    )
    tf = title_box.text_frame
    tf.paragraphs[0].text = title
    tf.paragraphs[0].font.size = Pt(24)
    tf.paragraphs[0].font.bold = True
    tf.paragraphs[0].font.color.rgb = style['title']

    # VPC 박스 (최하단 레이어)
    if "vpc" in config:
        v = config["vpc"]
        add_vpc_box(slide, v["x"], v["y"], v["width"], v["height"],
                    v.get("label", "VPC"), style)

    # Subnet 박스
    for subnet in config.get("subnets", []):
        add_subnet_box(slide, subnet["x"], subnet["y"],
                       subnet["width"], subnet["height"],
                       subnet["label"], subnet["public"], style)

    # 서비스 아이콘
    for svc in config.get("services", []):
        add_aws_service(slide, svc["x"], svc["y"],
                        svc.get("icon"), svc["label"], style,
                        category=svc.get("cat", "compute"))

    # 화살표
    for arr in config.get("arrows", []):
        start = arr["from"]
        end = arr["to"]
        add_arrow(slide, start[0], start[1], end[0], end[1], style)

    return slide
```

## 사용 예시

```python
# 3-tier 웹 아키텍처
add_aws_architecture_slide(
    prs,
    title="AWS 3-Tier Architecture",
    config={
        "vpc": {"x": 1.5, "y": 1.8, "width": 11, "height": 5, "label": "VPC (10.0.0.0/16)"},
        "subnets": [
            {"x": 1.7, "y": 2.2, "width": 5, "height": 4.4, "label": "Public Subnet (10.0.1.0/24)", "public": True},
            {"x": 7, "y": 2.2, "width": 5.3, "height": 4.4, "label": "Private Subnet (10.0.2.0/24)", "public": False},
        ],
        "services": [
            # Public
            {"x": 2.5, "y": 3, "icon": "aws-icons/networking/elb.png", "label": "ALB", "cat": "networking"},
            {"x": 4.5, "y": 3, "icon": "aws-icons/compute/ec2.png", "label": "Web (EC2)", "cat": "compute"},
            # Private
            {"x": 7.8, "y": 3, "icon": "aws-icons/compute/ec2.png", "label": "App (EC2)", "cat": "compute"},
            {"x": 9.8, "y": 3, "icon": "aws-icons/database/rds.png", "label": "RDS", "cat": "database"},
            {"x": 9.8, "y": 4.8, "icon": "aws-icons/database/elasticache.png", "label": "Redis", "cat": "database"},
        ],
        "arrows": [
            {"from": [3.1, 3.5], "to": [4.5, 3.5]},
            {"from": [5.7, 3.5], "to": [7.8, 3.5]},
            {"from": [9, 3.5], "to": [9.8, 3.5]},
        ]
    },
    style=aws_style,
    header_func=add_header,
    page_num=3
)
```

## 아이콘 없이 사용

아이콘이 없어도 카테고리 색상으로 구분 가능:

```python
# 아이콘 없이 색상으로만
add_aws_service(slide, 3, 3, None, "EC2", aws_style, category="compute")     # 주황 테두리
add_aws_service(slide, 5, 3, None, "RDS", aws_style, category="database")    # 파랑 테두리
add_aws_service(slide, 7, 3, None, "S3", aws_style, category="storage")      # 초록 테두리
```

## 다른 클라우드

- **GCP Icons**: https://cloud.google.com/icons
- **Azure Icons**: https://learn.microsoft.com/azure/architecture/icons/
- 동일한 패턴으로 `gcp_style`, `azure_style` 정의 가능

---

## 자동 레이아웃 AWS 아키텍처

복잡한 AWS 아키텍처의 겹침 방지를 위해 레이아웃 엔진 사용.

**참조**: `resources/04-layout-engine.md`

### 자동 배치 함수

```python
from layout_engine import DiagramGrid, ArrowRouter, ElementPorts

def add_aws_auto_layout_slide(prs, title, services, connections, style=None, header_func=None, page_num=1,
                               vpc_config=None, subnets=None):
    """
    자동 레이아웃 AWS 아키텍처

    Args:
        services: [
            {"id": "alb", "label": "ALB", "category": "networking"},
            {"id": "ec2_1", "label": "EC2", "category": "compute"},
            {"id": "rds", "label": "RDS", "category": "database"},
        ]
        connections: [
            {"from": "alb", "to": "ec2_1"},
            {"from": "ec2_1", "to": "rds"},
        ]
        vpc_config: {"label": "VPC (10.0.0.0/16)"}  # 선택
        subnets: [
            {"label": "Public Subnet", "public": True, "services": ["alb", "ec2_1"]},
            {"label": "Private Subnet", "public": False, "services": ["rds"]},
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
        Inches(12), Inches(0.5)
    )
    tf = title_box.text_frame
    tf.paragraphs[0].text = title
    tf.paragraphs[0].font.size = Pt(24)
    tf.paragraphs[0].font.bold = True
    tf.paragraphs[0].font.color.rgb = style['title']

    # 그리드 초기화 (VPC 영역 내)
    grid = DiagramGrid(cols=12, rows=6, margin=0.7, header_height=1.5)
    router = ArrowRouter(grid)

    # Subnet 영역 계산 (있는 경우)
    if subnets:
        subnet_regions = _calculate_subnet_regions(subnets, grid)
        for subnet in subnets:
            region = subnet_regions[subnet["label"]]
            add_subnet_box(slide, region["x"], region["y"],
                          region["width"], region["height"],
                          subnet["label"], subnet["public"], style)

    # VPC 박스 (최하단 레이어)
    if vpc_config:
        add_vpc_box(slide, 1.5, 1.8, 11, 5,
                    vpc_config.get("label", "VPC"), style)

    # 서비스 자동 배치
    for svc in services:
        svc_id = svc["id"]
        # Subnet 소속 확인
        target_subnet = None
        if subnets:
            for subnet in subnets:
                if svc_id in subnet.get("services", []):
                    target_subnet = subnet["label"]
                    break

        # 빈 위치 찾기 (해당 서브넷 영역 내)
        pos = grid.find_free_position(1, 1, region=target_subnet)
        if pos:
            grid.place(svc_id, pos[0], pos[1], 1, 1)

    # 포트 시스템 초기화
    ports = {}
    for svc_id, data in grid.elements.items():
        x, y = grid.to_inches(data["row"], data["col"])
        ports[svc_id] = ElementPorts((x, y), (grid.cell_width, grid.cell_height))

    # 연결별 포트 할당
    port_usage = _assign_ports(connections, grid, ports)

    # 서비스 아이콘/박스 그리기
    for svc in services:
        data = grid.elements[svc["id"]]
        x, y = grid.to_inches(data["row"], data["col"])
        add_aws_service(slide, x, y, svc.get("icon"),
                        svc["label"], style, category=svc.get("category", "compute"))

    # 화살표 라우팅
    for conn in connections:
        from_id = conn["from"]
        to_id = conn["to"]

        # 포트 위치 가져오기
        from_port = port_usage[from_id][to_id]["from"]
        to_port = port_usage[from_id][to_id]["to"]

        # 직교 경로 계산
        path = router.route(from_id, to_id, from_port=from_port, to_port=to_port)
        _draw_aws_arrow(slide, path, style)

    return slide


def _calculate_subnet_regions(subnets, grid):
    """서브넷 영역 계산"""
    regions = {}
    num_subnets = len(subnets)

    # 수평 분할
    subnet_width = (grid.content_width - 0.4) / num_subnets
    for i, subnet in enumerate(subnets):
        regions[subnet["label"]] = {
            "x": 1.7 + i * (subnet_width + 0.2),
            "y": 2.2,
            "width": subnet_width,
            "height": 4.4,
            "cols": (i * 6, (i + 1) * 6)  # 그리드 열 범위
        }
    return regions


def _assign_ports(connections, grid, ports):
    """연결별 포트 자동 할당 (겹침 방지)"""
    usage = {}
    outgoing_count = {}  # {element_id: {"right": 0, "bottom": 0, ...}}

    for conn in connections:
        from_id = conn["from"]
        to_id = conn["to"]

        outgoing_count.setdefault(from_id, {"right": 0, "left": 0, "top": 0, "bottom": 0})
        outgoing_count.setdefault(to_id, {"right": 0, "left": 0, "top": 0, "bottom": 0})

        # 방향 결정
        from_data = grid.elements[from_id]
        to_data = grid.elements[to_id]

        if to_data["col"] > from_data["col"]:  # 오른쪽으로
            from_side, to_side = "right", "left"
        elif to_data["col"] < from_data["col"]:  # 왼쪽으로
            from_side, to_side = "left", "right"
        elif to_data["row"] > from_data["row"]:  # 아래로
            from_side, to_side = "bottom", "top"
        else:  # 위로
            from_side, to_side = "top", "bottom"

        # 포트 인덱스 할당
        from_port_idx = outgoing_count[from_id][from_side]
        to_port_idx = outgoing_count[to_id][to_side]

        outgoing_count[from_id][from_side] += 1
        outgoing_count[to_id][to_side] += 1

        # 포트 위치 계산
        from_port = ports[from_id].get_port_position(from_side, from_port_idx)
        to_port = ports[to_id].get_port_position(to_side, to_port_idx)

        usage.setdefault(from_id, {})
        usage[from_id][to_id] = {"from": from_port, "to": to_port}

    return usage


def _draw_aws_arrow(slide, path, style):
    """AWS 스타일 화살표 그리기"""
    for i in range(len(path) - 1):
        x1, y1 = path[i]
        x2, y2 = path[i + 1]
        add_arrow(slide, x1, y1, x2, y2, style)

    # 화살표 머리
    if len(path) >= 2:
        x1, y1 = path[-2]
        x2, y2 = path[-1]
        if abs(x2 - x1) > abs(y2 - y1):
            direction = "right" if x2 > x1 else "left"
        else:
            direction = "down" if y2 > y1 else "up"
        add_arrow_head(slide, x2 - 0.08, y2 - 0.075, direction, style)
```

### 사용 예시 - 자동 레이아웃

```python
# Before: 수동 좌표 (겹침 위험)
config = {
    "services": [
        {"x": 3, "y": 3, "icon": "...", "label": "ALB"},
        {"x": 5, "y": 3, "icon": "...", "label": "EC2"},  # 겹칠 수 있음
    ]
}

# After: 자동 배치 (겹침 방지 + 포트 분배)
add_aws_auto_layout_slide(prs,
    title="3-Tier Architecture",
    services=[
        {"id": "alb", "label": "ALB", "category": "networking"},
        {"id": "ec2_1", "label": "EC2 #1", "category": "compute"},
        {"id": "ec2_2", "label": "EC2 #2", "category": "compute"},
        {"id": "rds", "label": "RDS", "category": "database"},
        {"id": "redis", "label": "ElastiCache", "category": "database"},
    ],
    connections=[
        {"from": "alb", "to": "ec2_1"},
        {"from": "alb", "to": "ec2_2"},  # ALB에서 2개 연결 → 포트 자동 분배
        {"from": "ec2_1", "to": "rds"},
        {"from": "ec2_2", "to": "rds"},
        {"from": "ec2_1", "to": "redis"},
    ],
    vpc_config={"label": "VPC (10.0.0.0/16)"},
    subnets=[
        {"label": "Public Subnet", "public": True, "services": ["alb", "ec2_1", "ec2_2"]},
        {"label": "Private Subnet", "public": False, "services": ["rds", "redis"]},
    ],
    style=aws_style,
    header_func=add_header,
    page_num=5
)
```

### ASCII 기반 AWS 아키텍처

```python
ascii_layout = """
                    +--------+
                    | Route53|
                    +--------+
                        |
    +-------------------+-------------------+
    |        VPC                            |
    |   +-------+     +-------+             |
    |   |  ALB  |---->|  EC2  |--+          |
    |   +-------+     +-------+  |          |
    |                      |     |          |
    |                      v     v          |
    |                 +-------+  +-------+  |
    |                 |  RDS  |  | Redis |  |
    |                 +-------+  +-------+  |
    +---------------------------------------+
"""

# ASCII에서 자동 파싱 후 배치
```

### Multi-Connection 예시

단일 컴포넌트에서 여러 연결이 나갈 때 자동 포트 분배:

```
[ALB]에서 3개 EC2로 연결 시:

     ┌────[EC2-1]
[ALB]┼────[EC2-2]    # 오른쪽 면 3개 포트 자동 분배
     └────[EC2-3]

→ ports["alb"].get_port_position("right", 0)  # 상단 포트
→ ports["alb"].get_port_position("right", 1)  # 중앙 포트
→ ports["alb"].get_port_position("right", 2)  # 하단 포트
```
