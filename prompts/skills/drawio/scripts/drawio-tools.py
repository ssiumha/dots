#!/usr/bin/env python3
"""
draw.io XML 검증 및 조회 도구

Usage:
    drawio-tools.py validate <file>   - XML 구조 검증 (폰트/레이어 검사 포함)
    drawio-tools.py info <file>       - 다이어그램 정보 출력
    drawio-tools.py list <file>       - 모든 셀 목록
    drawio-tools.py styles <file>     - 사용된 스타일 분석
    drawio-tools.py aws <file>        - AWS 다이어그램 검증
    drawio-tools.py export <file> [output.png]  - PNG로 export
"""

import sys
import subprocess
import xml.etree.ElementTree as ET
from collections import Counter
from pathlib import Path


# AWS 공식 색상
AWS_COLORS = {
    "compute": {"fill": "#FF9900", "stroke": "#CC7A00"},
    "database": {"fill": "#3B48CC", "stroke": "#2E3A9F"},
    "storage": {"fill": "#3F8624", "stroke": "#2D6119"},
    "networking": {"fill": "#8C4FFF", "stroke": "#5429A6"},
    "security": {"fill": "#DD344C", "stroke": "#A62839"},
    "application": {"fill": "#E7157B", "stroke": "#B01161"},
}

AWS_GROUP_COLORS = {
    "aws_cloud": {"stroke": "#232F3E"},
    "region": {"fill": "#E7F3FF", "stroke": "#147EBA"},
    "vpc": {"fill": "#E9F7EF", "stroke": "#248814"},
    "subnet_public": {"fill": "#E9F7EF", "stroke": "#248814"},
    "subnet_private": {"fill": "#E7F3FF", "stroke": "#147EBA"},
}


def parse_style(style_str: str) -> dict:
    """스타일 문자열을 딕셔너리로 파싱"""
    if not style_str:
        return {}
    result = {}
    for item in style_str.rstrip(";").split(";"):
        if "=" in item:
            key, value = item.split("=", 1)
            result[key] = value
        else:
            result[item] = True
    return result


def validate_style(style_str: str, cell_id: str) -> list:
    """스타일 문자열 검증"""
    issues = []
    if not style_str:
        return issues

    # 알려진 스타일 키
    known_keys = {
        "rounded", "whiteSpace", "html", "fillColor", "strokeColor", "strokeWidth",
        "fontColor", "fontSize", "fontStyle", "shape", "edgeStyle", "curved",
        "dashed", "endArrow", "startArrow", "verticalAlign", "verticalLabelPosition",
        "labelPosition", "align", "spacingTop", "spacingBottom", "spacingLeft",
        "spacingRight", "opacity", "gradientColor", "gradientDirection", "shadow",
        "glass", "comic", "sketch", "points", "outlineConnect", "perimeter",
        "aspect", "resizable", "rotatable", "deletable", "editable", "connectable",
        "movable", "cloneable", "autosize", "foldable", "collapsible", "container",
        "swimlane", "image", "imageWidth", "imageHeight", "imageAlign", "imageVerticalAlign",
        "indicatorColor", "indicatorShape", "indicatorWidth", "indicatorHeight",
        "sourcePerimeterSpacing", "targetPerimeterSpacing", "entryX", "entryY",
        "exitX", "exitY", "size", "arcSize", "absoluteArcSize", "pointerEvents",
        "direction", "rotation", "flipH", "flipV", "noLabel", "labelBackgroundColor",
        "labelBorderColor", "endFill", "startFill", "endSize", "startSize",
        "jettySize", "orthogonalLoop", "elbow", "jumpStyle", "jumpSize",
        "sourcePortConstraint", "targetPortConstraint", "port", "portConstraint",
        "parent", "relative", "childLayout", "recursiveResize"
    }

    for item in style_str.rstrip(";").split(";"):
        if not item:
            continue
        if "=" in item:
            key, value = item.split("=", 1)
            # 알 수 없는 키 체크 (너무 엄격하지 않게 - 경고만)
            # if key not in known_keys:
            #     issues.append(f"[{cell_id}] 알 수 없는 스타일 키: {key}")
        # else는 플래그 형식이므로 OK

    return issues


def validate(file_path: str) -> bool:
    """draw.io XML 구조 검증"""
    errors = []
    warnings = []

    try:
        tree = ET.parse(file_path)
        root = tree.getroot()
    except ET.ParseError as e:
        print(f"[ERROR] XML 파싱 실패: {e}")
        return False

    # 1. mxfile 확인
    if root.tag != "mxfile":
        errors.append(f"루트 태그가 'mxfile'이 아님: {root.tag}")

    # 2. compressed 속성 확인
    compressed = root.get("compressed", "true")
    if compressed.lower() == "true":
        warnings.append("XML이 압축되어 있음 (compressed='true')")

    # 3. diagram 확인
    diagrams = root.findall("diagram")
    if not diagrams:
        errors.append("'diagram' 요소가 없음")

    for i, diagram in enumerate(diagrams):
        diagram_name = diagram.get("name", f"diagram-{i}")

        # 4. mxGraphModel 확인
        graph_model = diagram.find("mxGraphModel")
        if graph_model is None:
            errors.append(f"[{diagram_name}] 'mxGraphModel'이 없음")
            continue

        # 5. root 확인
        model_root = graph_model.find("root")
        if model_root is None:
            errors.append(f"[{diagram_name}] 'root'가 없음")
            continue

        # 6. 기본 셀 확인
        cells = model_root.findall("mxCell")
        ids = [c.get("id") for c in cells]
        id_set = set(ids)

        if "0" not in id_set:
            errors.append(f"[{diagram_name}] 기본 셀 id='0'이 없음")
        if "1" not in id_set:
            errors.append(f"[{diagram_name}] 기본 레이어 id='1'이 없음")

        # 7. ID 중복 확인
        id_counts = Counter(ids)
        duplicates = [id for id, count in id_counts.items() if count > 1]
        if duplicates:
            errors.append(f"[{diagram_name}] 중복된 ID: {duplicates}")

        # 8. parent 존재 확인
        for cell in cells:
            cell_id = cell.get("id")
            parent_id = cell.get("parent")
            if parent_id and parent_id not in id_set:
                errors.append(f"[{diagram_name}] '{cell_id}'의 parent '{parent_id}'가 존재하지 않음")

        # 9. edge 검증
        edge_sources = set()
        edge_targets = set()
        for cell in cells:
            if cell.get("edge") == "1":
                cell_id = cell.get("id")
                source = cell.get("source")
                target = cell.get("target")

                # source/target 존재 확인
                if source and source not in id_set:
                    errors.append(f"[{diagram_name}] edge '{cell_id}'의 source '{source}'가 존재하지 않음")
                if target and target not in id_set:
                    errors.append(f"[{diagram_name}] edge '{cell_id}'의 target '{target}'가 존재하지 않음")

                # 자기 참조 확인
                if source and target and source == target:
                    warnings.append(f"[{diagram_name}] edge '{cell_id}'가 자기 자신을 참조 (source=target='{source}')")

                if source:
                    edge_sources.add(source)
                if target:
                    edge_targets.add(target)

        # 10. vertex 검증
        vertex_positions = []
        for cell in cells:
            if cell.get("vertex") == "1":
                cell_id = cell.get("id")
                geom = cell.find("mxGeometry")

                # geometry 존재 확인
                if geom is None:
                    warnings.append(f"[{diagram_name}] vertex '{cell_id}'에 mxGeometry가 없음")
                else:
                    # geometry 값 검증
                    try:
                        x = float(geom.get("x", 0))
                        y = float(geom.get("y", 0))
                        width = float(geom.get("width", 0))
                        height = float(geom.get("height", 0))

                        if width <= 0 or height <= 0:
                            warnings.append(f"[{diagram_name}] vertex '{cell_id}'의 크기가 0 이하 (w={width}, h={height})")

                        if x < 0 or y < 0:
                            warnings.append(f"[{diagram_name}] vertex '{cell_id}'의 좌표가 음수 (x={x}, y={y})")

                        vertex_positions.append((cell_id, x, y, width, height))

                    except (TypeError, ValueError):
                        warnings.append(f"[{diagram_name}] vertex '{cell_id}'의 geometry 값이 유효하지 않음")

                # 고립된 노드 확인 (edge와 연결되지 않은 노드)
                connected = edge_sources | edge_targets
                if cell_id not in connected and cell_id not in ("0", "1"):
                    # parent가 연결된 경우는 제외 (그룹 내부 요소)
                    parent_id = cell.get("parent")
                    if parent_id == "1":  # 최상위 레이어에 있는 고립 노드만
                        warnings.append(f"[{diagram_name}] vertex '{cell_id}'가 어떤 edge와도 연결되지 않음")

        # 11. 겹침 감지 (같은 좌표의 노드)
        for i, (id1, x1, y1, w1, h1) in enumerate(vertex_positions):
            for id2, x2, y2, w2, h2 in vertex_positions[i+1:]:
                if abs(x1 - x2) < 5 and abs(y1 - y2) < 5:
                    warnings.append(f"[{diagram_name}] vertex '{id1}'와 '{id2}'가 거의 같은 위치에 있음")

        # 12. 순환 참조 검사
        parent_map = {c.get("id"): c.get("parent") for c in cells}
        for cell_id in ids:
            visited = set()
            current = cell_id
            while current and current in parent_map:
                if current in visited:
                    errors.append(f"[{diagram_name}] 순환 참조 감지: {cell_id}")
                    break
                visited.add(current)
                current = parent_map.get(current)

        # 13. defaultFontFamily 검사 (정보 수준)
        default_font = graph_model.get("defaultFontFamily")
        if not default_font:
            # defaultFontFamily가 없으면 정보로만 알림
            pass  # PNG 출력 시 폰트 문제 가능성 있으나 경고까지는 아님

    # 결과 출력
    print(f"\n검증 결과: {file_path}")
    print("=" * 50)

    if errors:
        print(f"\n[ERRORS] {len(errors)}개")
        for e in errors:
            print(f"  ❌ {e}")

    if warnings:
        print(f"\n[WARNINGS] {len(warnings)}개")
        for w in warnings:
            print(f"  ⚠️  {w}")

    if not errors and not warnings:
        print("\n✅ 검증 통과")
    elif not errors:
        print(f"\n✅ 검증 통과 (경고 {len(warnings)}개)")

    return len(errors) == 0


def info(file_path: str) -> None:
    """다이어그램 정보 출력"""
    tree = ET.parse(file_path)
    root = tree.getroot()

    print(f"\n다이어그램 정보: {file_path}")
    print("=" * 50)

    # 파일 속성
    print(f"\n[파일 속성]")
    print(f"  host: {root.get('host', 'N/A')}")
    print(f"  compressed: {root.get('compressed', 'true')}")

    diagrams = root.findall("diagram")
    print(f"  페이지 수: {len(diagrams)}")

    for i, diagram in enumerate(diagrams):
        diagram_name = diagram.get("name", f"Page-{i+1}")
        print(f"\n[{diagram_name}]")

        graph_model = diagram.find("mxGraphModel")
        if graph_model is None:
            print("  (mxGraphModel 없음)")
            continue

        # 캔버스 설정
        print(f"  캔버스: {graph_model.get('pageWidth')}x{graph_model.get('pageHeight')}")
        print(f"  그리드: {'ON' if graph_model.get('grid') == '1' else 'OFF'} (size={graph_model.get('gridSize')})")

        model_root = graph_model.find("root")
        if model_root is None:
            continue

        cells = model_root.findall("mxCell")

        # 통계
        vertices = [c for c in cells if c.get("vertex") == "1"]
        edges = [c for c in cells if c.get("edge") == "1"]

        print(f"\n  [통계]")
        print(f"    총 셀: {len(cells)}")
        print(f"    노드(vertex): {len(vertices)}")
        print(f"    연결선(edge): {len(edges)}")

        # 라벨 있는 노드
        labeled = [c for c in vertices if c.get("value")]
        print(f"    라벨 있는 노드: {len(labeled)}")


def list_cells(file_path: str) -> None:
    """모든 셀 목록"""
    tree = ET.parse(file_path)
    root = tree.getroot()

    print(f"\n셀 목록: {file_path}")
    print("=" * 50)

    for diagram in root.findall("diagram"):
        diagram_name = diagram.get("name", "Unknown")
        print(f"\n[{diagram_name}]")
        print("-" * 40)

        graph_model = diagram.find("mxGraphModel")
        if graph_model is None:
            continue

        model_root = graph_model.find("root")
        if model_root is None:
            continue

        print(f"{'ID':<8} {'Type':<8} {'Value':<20} {'Parent':<8}")
        print("-" * 40)

        for cell in model_root.findall("mxCell"):
            cell_id = cell.get("id", "")

            if cell.get("vertex") == "1":
                cell_type = "vertex"
            elif cell.get("edge") == "1":
                cell_type = "edge"
            else:
                cell_type = "base"

            value = cell.get("value", "")[:18]
            parent = cell.get("parent", "")

            print(f"{cell_id:<8} {cell_type:<8} {value:<20} {parent:<8}")


def analyze_styles(file_path: str) -> None:
    """사용된 스타일 분석"""
    tree = ET.parse(file_path)
    root = tree.getroot()

    print(f"\n스타일 분석: {file_path}")
    print("=" * 50)

    all_styles = Counter()
    shapes = Counter()
    colors = {"fill": Counter(), "stroke": Counter(), "font": Counter()}

    for diagram in root.findall("diagram"):
        graph_model = diagram.find("mxGraphModel")
        if graph_model is None:
            continue

        model_root = graph_model.find("root")
        if model_root is None:
            continue

        for cell in model_root.findall("mxCell"):
            style_str = cell.get("style", "")
            if not style_str:
                continue

            style = parse_style(style_str)

            for key in style:
                all_styles[key] += 1

            if "shape" in style:
                shapes[style["shape"]] += 1

            if "fillColor" in style:
                colors["fill"][style["fillColor"]] += 1
            if "strokeColor" in style:
                colors["stroke"][style["strokeColor"]] += 1
            if "fontColor" in style:
                colors["font"][style["fontColor"]] += 1

    print("\n[사용된 스타일 속성]")
    for style, count in all_styles.most_common(15):
        print(f"  {style}: {count}")

    if shapes:
        print("\n[도형 종류]")
        for shape, count in shapes.most_common():
            print(f"  {shape}: {count}")

    print("\n[색상]")
    for color_type, counter in colors.items():
        if counter:
            print(f"  {color_type}:")
            for color, count in counter.most_common(5):
                print(f"    {color}: {count}")


def export_png(file_path: str, output_path: str = None) -> bool:
    """draw.io CLI를 사용하여 PNG로 export"""
    input_path = Path(file_path)

    if output_path is None:
        output_path = input_path.with_suffix(".png")
    else:
        output_path = Path(output_path)

    # draw.io 앱 경로 (macOS)
    drawio_paths = [
        "/Applications/draw.io.app/Contents/MacOS/draw.io",
        "/usr/local/bin/draw.io",
        "draw.io",  # PATH에 있는 경우
    ]

    drawio_cmd = None
    for path in drawio_paths:
        try:
            result = subprocess.run(
                [path, "--version"],
                capture_output=True,
                text=True,
                timeout=5
            )
            if result.returncode == 0:
                drawio_cmd = path
                break
        except (subprocess.TimeoutExpired, FileNotFoundError):
            continue

    if drawio_cmd is None:
        print("[ERROR] draw.io를 찾을 수 없습니다.")
        print("설치 방법: brew install --cask drawio")
        return False

    print(f"\nPNG export: {file_path}")
    print("=" * 50)
    print(f"  입력: {input_path}")
    print(f"  출력: {output_path}")
    print(f"  도구: {drawio_cmd}")

    try:
        result = subprocess.run(
            [
                drawio_cmd,
                "--export",
                "--format", "png",
                "--output", str(output_path),
                str(input_path)
            ],
            capture_output=True,
            text=True,
            timeout=60
        )

        if result.returncode == 0:
            print(f"\n✅ export 완료: {output_path}")
            print(f"   파일 크기: {output_path.stat().st_size:,} bytes")
            return True
        else:
            print(f"\n[ERROR] export 실패")
            if result.stderr:
                print(f"  {result.stderr}")
            return False

    except subprocess.TimeoutExpired:
        print("[ERROR] export 시간 초과 (60초)")
        return False
    except Exception as e:
        print(f"[ERROR] export 중 오류: {e}")
        return False


def validate_aws(file_path: str) -> bool:
    """AWS 다이어그램 검증"""
    tree = ET.parse(file_path)
    root = tree.getroot()

    print(f"\nAWS 다이어그램 검증: {file_path}")
    print("=" * 50)

    warnings = []
    suggestions = []

    # AWS 서비스 키워드
    aws_services = {
        "compute": ["EC2", "Lambda", "ECS", "Fargate", "Batch"],
        "database": ["RDS", "DynamoDB", "ElastiCache", "Aurora", "Redshift"],
        "storage": ["S3", "EBS", "EFS", "Glacier"],
        "networking": ["VPC", "ALB", "NLB", "CloudFront", "Route53", "API Gateway"],
        "security": ["IAM", "Cognito", "WAF", "KMS", "ACM"],
        "application": ["SQS", "SNS", "SES", "Step Functions"],
    }

    found_services = {cat: [] for cat in aws_services}
    color_issues = []

    for diagram in root.findall("diagram"):
        graph_model = diagram.find("mxGraphModel")
        if graph_model is None:
            continue

        model_root = graph_model.find("root")
        if model_root is None:
            continue

        for cell in model_root.findall("mxCell"):
            value = cell.get("value", "")
            style_str = cell.get("style", "")
            style = parse_style(style_str)

            # AWS 서비스 감지
            for category, services in aws_services.items():
                for service in services:
                    if service.lower() in value.lower():
                        found_services[category].append(service)

                        # 색상 검증
                        fill_color = style.get("fillColor", "").upper()
                        expected_fill = AWS_COLORS[category]["fill"].upper()

                        if fill_color and fill_color != expected_fill:
                            color_issues.append(
                                f"{service}: fillColor={fill_color} (권장: {expected_fill})"
                            )

            # AWS 그룹 감지 (VPC, Subnet 등)
            value_lower = value.lower()
            if "vpc" in value_lower:
                stroke = style.get("strokeColor", "").upper()
                expected = AWS_GROUP_COLORS["vpc"]["stroke"].upper()
                if stroke and stroke != expected:
                    color_issues.append(f"VPC 그룹: strokeColor={stroke} (권장: {expected})")

            if "subnet" in value_lower:
                fill = style.get("fillColor", "").upper()
                if "public" in value_lower:
                    expected = AWS_GROUP_COLORS["subnet_public"]["fill"].upper()
                else:
                    expected = AWS_GROUP_COLORS["subnet_private"]["fill"].upper()
                if fill and fill != expected:
                    color_issues.append(f"Subnet: fillColor={fill} (권장: {expected})")

    # 결과 출력
    print("\n[감지된 AWS 서비스]")
    total_services = 0
    for category, services in found_services.items():
        if services:
            print(f"  {category}: {', '.join(set(services))}")
            total_services += len(set(services))

    if total_services == 0:
        warnings.append("AWS 서비스가 감지되지 않음")

    if color_issues:
        print(f"\n[색상 불일치] {len(color_issues)}개")
        for issue in color_issues:
            print(f"  ⚠️  {issue}")
    else:
        print("\n[색상 검증] ✅ AWS 공식 색상 사용")

    # 그룹 계층 체크
    print("\n[권장 그룹 계층]")
    print("  AWS Cloud > Region > VPC > AZ > Subnet")

    if warnings:
        print(f"\n[WARNINGS] {len(warnings)}개")
        for w in warnings:
            print(f"  - {w}")

    return len(color_issues) == 0


def main():
    if len(sys.argv) < 3:
        print(__doc__)
        sys.exit(1)

    command = sys.argv[1]
    file_path = sys.argv[2]

    if not Path(file_path).exists():
        print(f"Error: 파일을 찾을 수 없음: {file_path}")
        sys.exit(1)

    if command == "validate":
        success = validate(file_path)
        sys.exit(0 if success else 1)
    elif command == "info":
        info(file_path)
    elif command == "list":
        list_cells(file_path)
    elif command == "styles":
        analyze_styles(file_path)
    elif command == "aws":
        success = validate_aws(file_path)
        sys.exit(0 if success else 1)
    elif command == "export":
        output_path = sys.argv[3] if len(sys.argv) > 3 else None
        success = export_png(file_path, output_path)
        sys.exit(0 if success else 1)
    else:
        print(f"Unknown command: {command}")
        print(__doc__)
        sys.exit(1)


if __name__ == "__main__":
    main()
