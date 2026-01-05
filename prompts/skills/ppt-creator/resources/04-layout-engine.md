# Layout Engine

다이어그램 요소 자동 배치 및 화살표 라우팅 엔진.

## 개요

```
┌─────────────────────────────────────────────────┐
│                                                 │
│   [A]──┬──────[B]     자동 레이아웃:            │
│        │       │      - 그리드 기반 배치        │
│        └──[C]──┘      - 포트별 연결 분산        │
│                       - 직교 화살표 라우팅      │
└─────────────────────────────────────────────────┘
```

## 1. 그리드 시스템

```python
class DiagramGrid:
    """
    슬라이드를 그리드로 분할하여 요소 배치 관리

    기본 그리드: 12열 x 6행 (헤더/푸터 제외 영역)
    """

    def __init__(self, cols=12, rows=6, margin=0.7, header_height=1.0, footer_height=0.5):
        self.cols = cols
        self.rows = rows
        self.margin = margin

        # 콘텐츠 영역 계산 (헤더/푸터 제외)
        self.content_left = margin
        self.content_top = header_height
        self.content_width = 13.333 - 2 * margin
        self.content_height = 7.5 - header_height - footer_height

        # 셀 크기
        self.cell_width = self.content_width / cols
        self.cell_height = self.content_height / rows

        # 점유 맵: {(row, col): element_id}
        self.occupied = {}

        # 요소 정보: {element_id: {"row": r, "col": c, "w": w, "h": h}}
        self.elements = {}

    def place(self, element_id, row, col, width_cells=1, height_cells=1):
        """
        요소 배치

        Args:
            element_id: 요소 식별자 (예: "ec2", "rds")
            row, col: 그리드 좌표 (0-indexed)
            width_cells, height_cells: 차지하는 셀 수
        """
        # 범위 체크
        if row + height_cells > self.rows or col + width_cells > self.cols:
            raise ValueError(f"Out of grid bounds: ({row}, {col}) + ({height_cells}, {width_cells})")

        # 충돌 체크
        for r in range(row, row + height_cells):
            for c in range(col, col + width_cells):
                if (r, c) in self.occupied and self.occupied[(r, c)] != element_id:
                    raise ValueError(f"Cell ({r}, {c}) already occupied by {self.occupied[(r, c)]}")

        # 배치
        for r in range(row, row + height_cells):
            for c in range(col, col + width_cells):
                self.occupied[(r, c)] = element_id

        self.elements[element_id] = {
            "row": row, "col": col,
            "width": width_cells, "height": height_cells
        }

    def find_free_position(self, width_cells=1, height_cells=1, preferred_row=None, preferred_col=None):
        """
        빈 위치 자동 탐색 (왼쪽→오른쪽, 위→아래 순서)

        Returns:
            (row, col) 또는 None
        """
        start_row = preferred_row if preferred_row is not None else 0
        start_col = preferred_col if preferred_col is not None else 0

        for row in range(start_row, self.rows - height_cells + 1):
            for col in range(start_col if row == start_row else 0, self.cols - width_cells + 1):
                if self._is_free(row, col, width_cells, height_cells):
                    return (row, col)
        return None

    def _is_free(self, row, col, width_cells, height_cells):
        """영역이 비어있는지 확인"""
        for r in range(row, row + height_cells):
            for c in range(col, col + width_cells):
                if (r, c) in self.occupied:
                    return False
        return True

    def to_inches(self, row, col):
        """그리드 좌표 → Inches 좌표 변환"""
        x = self.content_left + col * self.cell_width
        y = self.content_top + row * self.cell_height
        return (x, y)

    def get_element_bounds(self, element_id):
        """요소의 실제 Inches 좌표 및 크기 반환"""
        elem = self.elements[element_id]
        x, y = self.to_inches(elem["row"], elem["col"])
        w = elem["width"] * self.cell_width
        h = elem["height"] * self.cell_height
        return {"x": x, "y": y, "width": w, "height": h}

    def get_center(self, element_id):
        """요소 중심점 반환"""
        bounds = self.get_element_bounds(element_id)
        cx = bounds["x"] + bounds["width"] / 2
        cy = bounds["y"] + bounds["height"] / 2
        return (cx, cy)
```

## 2. 포트 시스템

```python
class ElementPorts:
    """
    요소의 연결 포트 관리
    각 면(top/right/bottom/left)에 여러 포트 지원
    """

    def __init__(self, bounds, max_ports_per_side=5):
        """
        Args:
            bounds: {"x": x, "y": y, "width": w, "height": h}
        """
        self.bounds = bounds
        self.max_ports = max_ports_per_side

        # 사용된 포트 추적: {"right": [0, 2], "left": [1]}
        self.used_ports = {"top": [], "right": [], "bottom": [], "left": []}

    def get_port_position(self, side, port_index=None):
        """
        포트 위치 반환

        Args:
            side: "top", "right", "bottom", "left"
            port_index: 특정 포트 (None이면 자동 할당)

        Returns:
            (x, y) Inches 좌표
        """
        b = self.bounds

        if port_index is None:
            # 자동 할당: 사용되지 않은 포트 중 중앙에 가장 가까운 것
            used = set(self.used_ports[side])
            for i in self._port_order():
                if i not in used:
                    port_index = i
                    break
            if port_index is None:
                port_index = len(self.used_ports[side]) % self.max_ports

        self.used_ports[side].append(port_index)

        # 포트 상대 위치 (0~1)
        ratio = (port_index + 1) / (self.max_ports + 1)

        if side == "top":
            return (b["x"] + b["width"] * ratio, b["y"])
        elif side == "bottom":
            return (b["x"] + b["width"] * ratio, b["y"] + b["height"])
        elif side == "left":
            return (b["x"], b["y"] + b["height"] * ratio)
        elif side == "right":
            return (b["x"] + b["width"], b["y"] + b["height"] * ratio)

    def _port_order(self):
        """중앙부터 번갈아가며 할당 (2, 1, 3, 0, 4 for max_ports=5)"""
        mid = self.max_ports // 2
        order = [mid]
        for i in range(1, mid + 1):
            if mid - i >= 0:
                order.append(mid - i)
            if mid + i < self.max_ports:
                order.append(mid + i)
        return order

    def best_side_for(self, target_x, target_y):
        """대상 위치에 가장 적합한 면 반환"""
        cx = self.bounds["x"] + self.bounds["width"] / 2
        cy = self.bounds["y"] + self.bounds["height"] / 2

        dx = target_x - cx
        dy = target_y - cy

        # 수평/수직 중 더 먼 방향 선택
        if abs(dx) > abs(dy):
            return "right" if dx > 0 else "left"
        else:
            return "bottom" if dy > 0 else "top"
```

## 3. 화살표 라우팅

```python
class ArrowRouter:
    """
    직교(orthogonal) 화살표 라우팅
    - 수평/수직 선분만 사용
    - 장애물 회피
    - 수동 waypoint 지원
    """

    def __init__(self, grid):
        self.grid = grid
        self.routes = []  # 계산된 경로 저장

    def route(self, from_id, to_id, waypoints=None, from_port=None, to_port=None):
        """
        두 요소 간 경로 계산

        Args:
            from_id, to_id: 요소 ID
            waypoints: [(x, y), ...] 수동 지정 꺾임점 (선택)
            from_port: (x, y) 출발 포트 위치 (선택, 명시적 지정)
            to_port: (x, y) 도착 포트 위치 (선택, 명시적 지정)

        Returns:
            [(x1, y1), (x2, y2), ...] 경로 점들
        """
        # 명시적 포트가 지정되면 그대로 사용
        if from_port and to_port:
            from_point = from_port
            to_point = to_port
            from_side = "right"  # 기본값 (경로 계산용)
            to_side = "left"
        else:
            from_bounds = self.grid.get_element_bounds(from_id)
            to_bounds = self.grid.get_element_bounds(to_id)

            from_ports = ElementPorts(from_bounds)
            to_ports = ElementPorts(to_bounds)

            # 출발/도착 포트 결정
            to_center = self.grid.get_center(to_id)
            from_side = from_ports.best_side_for(*to_center)
            from_point = from_ports.get_port_position(from_side)

            from_center = self.grid.get_center(from_id)
            to_side = to_ports.best_side_for(*from_center)
            to_point = to_ports.get_port_position(to_side)

        # 수동 waypoints가 있으면 그대로 사용
        if waypoints:
            path = [from_point] + waypoints + [to_point]
        else:
            # 자동 경로 계산
            path = self._calculate_path(from_point, to_point, from_side, to_side)

        self.routes.append({
            "from": from_id,
            "to": to_id,
            "path": path
        })

        return path

    def _calculate_path(self, start, end, from_side, to_side):
        """
        자동 경로 계산 (직교 라우팅)
        """
        sx, sy = start
        ex, ey = end

        # Case 1: 직선 가능 (수평)
        if abs(sy - ey) < 0.1 and self._horizontal_clear(sy, sx, ex):
            return [start, end]

        # Case 2: 직선 가능 (수직)
        if abs(sx - ex) < 0.1 and self._vertical_clear(sx, sy, ey):
            return [start, end]

        # Case 3: ㄱ자 또는 ㄴ자 경로
        # 출발 방향에 따라 먼저 나가는 방향 결정
        if from_side in ("left", "right"):
            # 수평 먼저
            mid_x = (sx + ex) / 2
            waypoint = (mid_x, sy)
            waypoint2 = (mid_x, ey)
            return [start, waypoint, waypoint2, end]
        else:
            # 수직 먼저
            mid_y = (sy + ey) / 2
            waypoint = (sx, mid_y)
            waypoint2 = (ex, mid_y)
            return [start, waypoint, waypoint2, end]

    def _horizontal_clear(self, y, x1, x2):
        """수평선이 장애물과 겹치는지 체크"""
        min_x, max_x = min(x1, x2), max(x1, x2)
        for elem_id, elem in self.grid.elements.items():
            bounds = self.grid.get_element_bounds(elem_id)
            # Y 범위 체크
            if bounds["y"] < y < bounds["y"] + bounds["height"]:
                # X 범위 겹침 체크
                if not (max_x < bounds["x"] or min_x > bounds["x"] + bounds["width"]):
                    return False
        return True

    def _vertical_clear(self, x, y1, y2):
        """수직선이 장애물과 겹치는지 체크"""
        min_y, max_y = min(y1, y2), max(y1, y2)
        for elem_id, elem in self.grid.elements.items():
            bounds = self.grid.get_element_bounds(elem_id)
            # X 범위 체크
            if bounds["x"] < x < bounds["x"] + bounds["width"]:
                # Y 범위 겹침 체크
                if not (max_y < bounds["y"] or min_y > bounds["y"] + bounds["height"]):
                    return False
        return True


def route_with_waypoints(from_id, to_id, waypoints):
    """
    수동 waypoint 지정 헬퍼

    예시:
    route_with_waypoints("alb", "rds", [(5, 3), (5, 5)])
    → ALB에서 (5,3)으로 → (5,5)로 → RDS로 연결
    """
    pass  # router.route() 호출
```

## 4. 통합 사용

```python
def create_auto_layout_diagram(prs, title, services, connections, style, header_func=None, page_num=1):
    """
    자동 레이아웃 다이어그램 생성

    Args:
        services: [
            {"id": "alb", "label": "ALB", "row": 1},  # row 힌트 (선택)
            {"id": "ec2", "label": "EC2"},
            {"id": "rds", "label": "RDS"},
        ]
        connections: [
            {"from": "alb", "to": "ec2"},
            {"from": "ec2", "to": "rds"},
            {"from": "alb", "to": "rds", "waypoints": [(6, 4)]},  # 수동 waypoint
        ]
    """
    slide = prs.slides.add_slide(prs.slide_layouts[6])

    if header_func:
        header_func(slide, title, style)

    # 1. 그리드 생성
    grid = DiagramGrid()

    # 2. 서비스 배치
    for svc in services:
        if "row" in svc and "col" in svc:
            # 수동 위치
            grid.place(svc["id"], svc["row"], svc["col"])
        else:
            # 자동 위치
            pos = grid.find_free_position(
                preferred_row=svc.get("row"),
                preferred_col=svc.get("col")
            )
            if pos:
                grid.place(svc["id"], pos[0], pos[1])

    # 3. 서비스 박스 그리기
    for svc in services:
        bounds = grid.get_element_bounds(svc["id"])
        add_box(slide, bounds["x"], bounds["y"],
                bounds["width"], bounds["height"],
                svc["label"], style)

    # 4. 연결선 라우팅 및 그리기
    router = ArrowRouter(grid)
    for conn in connections:
        path = router.route(
            conn["from"],
            conn["to"],
            waypoints=conn.get("waypoints")
        )
        draw_path(slide, path, style)

    return slide


def draw_path(slide, path, style):
    """경로를 연속된 선분으로 그리기"""
    for i in range(len(path) - 1):
        x1, y1 = path[i]
        x2, y2 = path[i + 1]

        line = slide.shapes.add_connector(
            MSO_CONNECTOR.STRAIGHT,
            Inches(x1), Inches(y1),
            Inches(x2), Inches(y2)
        )
        line.line.color.rgb = style.get("arrow", style["text"])
        line.line.width = Pt(2)

    # 마지막 점에 화살표 머리
    if len(path) >= 2:
        x1, y1 = path[-2]
        x2, y2 = path[-1]

        # 방향 판단 후 화살표 머리 추가
        if x2 > x1:
            add_arrow_head(slide, x2 - 0.08, y2 - 0.075, "right", style)
        elif x2 < x1:
            add_arrow_head(slide, x2, y2 - 0.075, "left", style)
        elif y2 > y1:
            add_arrow_head(slide, x2 - 0.075, y2 - 0.08, "down", style)
        else:
            add_arrow_head(slide, x2 - 0.075, y2, "up", style)
```

## 5. 사용 예시

### 자동 레이아웃

```python
create_auto_layout_diagram(
    prs,
    title="시스템 아키텍처",
    services=[
        {"id": "client", "label": "Client", "row": 0},
        {"id": "alb", "label": "ALB", "row": 2},
        {"id": "ec2_1", "label": "EC2-1", "row": 2},
        {"id": "ec2_2", "label": "EC2-2", "row": 2},
        {"id": "rds", "label": "RDS", "row": 4},
    ],
    connections=[
        {"from": "client", "to": "alb"},
        {"from": "alb", "to": "ec2_1"},
        {"from": "alb", "to": "ec2_2"},
        {"from": "ec2_1", "to": "rds"},
        {"from": "ec2_2", "to": "rds"},
    ],
    style=corporate_style,
    header_func=add_header
)
```

### 수동 waypoint

```python
connections=[
    # 직선으로 가면 다른 박스와 겹칠 때
    {"from": "alb", "to": "cache", "waypoints": [(8, 2), (8, 5)]},
]
```

### 혼합 (일부 수동 위치 + 자동)

```python
services=[
    {"id": "alb", "label": "ALB", "row": 1, "col": 2},  # 수동 위치
    {"id": "ec2", "label": "EC2"},                       # 자동 위치
]
```

## 6. ASCII 기반 레이아웃 정의

사용자가 ASCII art로 레이아웃을 정의하면 자동 파싱:

### 입력 형식

```python
ascii_layout = """
#  ASCII 다이어그램 정의
#  [name] = 박스
#  --> = 수평 화살표
#  |, v, ^ = 수직 화살표

         [Client]
             |
             v
[WAF] --> [ALB] --> [EC2-1]
             |          |
             v          v
         [Cache]    [RDS]
"""
```

### 파서 구현

```python
import re

class AsciiLayoutParser:
    """ASCII art를 그리드 좌표로 변환"""

    def __init__(self, ascii_text):
        self.lines = ascii_text.strip().split('\n')
        self.elements = {}      # {name: {"row": r, "col": c}}
        self.connections = []   # [{"from": a, "to": b}]

    def parse(self):
        """ASCII 파싱하여 요소와 연결 추출"""
        # 1. 박스 추출 [name]
        box_pattern = r'\[([^\]]+)\]'

        for row_idx, line in enumerate(self.lines):
            # 주석 무시
            if line.strip().startswith('#'):
                continue

            for match in re.finditer(box_pattern, line):
                name = match.group(1)
                col_idx = match.start() // 6  # 대략적인 컬럼 위치
                self.elements[name] = {
                    "row": row_idx // 2,  # 2줄당 1행
                    "col": col_idx,
                    "label": name
                }

        # 2. 수평 연결 추출 -->
        for row_idx, line in enumerate(self.lines):
            # [A] --> [B] 패턴
            h_pattern = r'\[([^\]]+)\]\s*-->\s*\[([^\]]+)\]'
            for match in re.finditer(h_pattern, line):
                self.connections.append({
                    "from": match.group(1),
                    "to": match.group(2)
                })

        # 3. 수직 연결 추출 (| v ^)
        self._parse_vertical_connections()

        return {
            "elements": list(self.elements.values()),
            "connections": self.connections
        }

    def _parse_vertical_connections(self):
        """수직 연결선 추적"""
        # 각 컬럼별로 | 또는 v 추적
        for col in range(max(len(line) for line in self.lines)):
            prev_box = None
            for row_idx, line in enumerate(self.lines):
                if col >= len(line):
                    continue

                char = line[col]

                # 박스 찾기
                box_match = re.search(r'\[([^\]]+)\]', line)
                if box_match and box_match.start() <= col <= box_match.end():
                    current_box = box_match.group(1)
                    if prev_box and prev_box != current_box:
                        self.connections.append({
                            "from": prev_box,
                            "to": current_box
                        })
                    prev_box = current_box

                # 수직선
                elif char in '|v^':
                    pass  # 연결 유지

                else:
                    prev_box = None  # 연결 끊김


def parse_ascii_layout(ascii_text):
    """
    ASCII 레이아웃을 파싱하여 create_auto_layout_diagram에 전달 가능한 형식으로 변환

    Returns:
        {
            "services": [...],
            "connections": [...]
        }
    """
    parser = AsciiLayoutParser(ascii_text)
    result = parser.parse()

    services = []
    for name, elem in parser.elements.items():
        services.append({
            "id": name.lower().replace("-", "_"),
            "label": name,
            "row": elem["row"],
            "col": elem["col"]
        })

    connections = []
    for conn in result["connections"]:
        connections.append({
            "from": conn["from"].lower().replace("-", "_"),
            "to": conn["to"].lower().replace("-", "_")
        })

    return {"services": services, "connections": connections}
```

### 사용 예시

```python
# ASCII로 레이아웃 정의
layout = parse_ascii_layout("""
            [Client]
                |
                v
    [WAF] --> [ALB] --> [EC2-1]
                |           |
                v           v
            [Redis]      [RDS]
""")

# 다이어그램 생성
create_auto_layout_diagram(
    prs,
    title="시스템 아키텍처",
    services=layout["services"],
    connections=layout["connections"],
    style=aws_style,
    header_func=add_header
)
```

### 주석으로 레이아웃 힌트

```python
def create_diagram_from_docstring(prs, title, func, style):
    """
    함수의 docstring에서 ASCII 레이아웃 추출

    Example:
        def my_architecture():
            '''
            [Client] --> [API] --> [DB]
            '''
            pass

        create_diagram_from_docstring(prs, "My Arch", my_architecture, style)
    """
    docstring = func.__doc__
    if docstring:
        layout = parse_ascii_layout(docstring)
        return create_auto_layout_diagram(prs, title, **layout, style=style)
```

### 복잡한 예시

```python
complex_layout = """
# AWS 3-Tier Architecture
# ========================

                    [Internet]
                        |
                        v
                    [Route53]
                        |
                        v
    [CloudFront] --> [ALB] --> [WAF]
                        |
           +------------+------------+
           |            |            |
           v            v            v
       [EC2-1]      [EC2-2]      [EC2-3]
           |            |            |
           +------+-----+-----+------+
                  |           |
                  v           v
               [RDS]      [ElastiCache]
                  |
                  v
              [S3-Backup]
"""

layout = parse_ascii_layout(complex_layout)
# → 10개 서비스, 14개 연결 자동 추출
```

## 그리드 시각화 (디버깅용)

```python
def visualize_grid(grid):
    """그리드 상태 출력 (디버깅용)"""
    for row in range(grid.rows):
        line = ""
        for col in range(grid.cols):
            if (row, col) in grid.occupied:
                line += f"[{grid.occupied[(row, col)][:3]}]"
            else:
                line += "[   ]"
        print(line)
```

```
출력 예시:
[   ][cli][   ][   ][   ][   ][   ][   ][   ][   ][   ][   ]
[   ][   ][   ][   ][   ][   ][   ][   ][   ][   ][   ][   ]
[   ][   ][alb][   ][ec2][   ][ec2][   ][   ][   ][   ][   ]
[   ][   ][   ][   ][   ][   ][   ][   ][   ][   ][   ][   ]
[   ][   ][   ][   ][   ][rds][   ][   ][   ][   ][   ][   ]
[   ][   ][   ][   ][   ][   ][   ][   ][   ][   ][   ][   ]
```
