# Chart Slide

차트 중심 슬라이드. python-pptx 차트 기능 활용.

## 레이아웃

```
┌─────────────────────────────────────────────────┐
│ [Header]                                        │
├─────────────────────────────────────────────────┤
│   Chart Title                                   │
│ ┌─────────────────────────────────────────────┐ │
│ │                                             │ │
│ │              [Chart Area]                   │ │
│ │                                             │ │
│ └─────────────────────────────────────────────┘ │
│   * Key insight or note                         │
├─────────────────────────────────────────────────┤
│ [Footer]                                        │
└─────────────────────────────────────────────────┘
```

## 코드

```python
from pptx.chart.data import CategoryChartData
from pptx.enum.chart import XL_CHART_TYPE

def add_chart_slide(prs, title, chart_type, categories, series_data,
                    note="", style=None, header_func=None, page_num=1):
    """
    차트 슬라이드

    Args:
        chart_type: "bar", "column", "line", "pie"
        categories: ["Q1", "Q2", "Q3", "Q4"]
        series_data: [
            {"name": "2024", "values": [10, 20, 30, 40]},
            {"name": "2025", "values": [15, 25, 35, 45]}
        ]
        note: 하단 주석
    """
    blank_layout = prs.slide_layouts[6]
    slide = prs.slides.add_slide(blank_layout)

    if header_func:
        header_func(slide, title, style)
    add_footer(slide, page_num, style)

    content_top = 1.0

    # 차트 제목
    title_box = slide.shapes.add_textbox(
        Inches(0.7), Inches(content_top),
        Inches(12), Inches(0.6)
    )
    tf = title_box.text_frame
    tf.paragraphs[0].text = title
    tf.paragraphs[0].font.size = Pt(28)
    tf.paragraphs[0].font.bold = True
    tf.paragraphs[0].font.color.rgb = style['title']

    # 차트 타입 매핑
    chart_types = {
        "bar": XL_CHART_TYPE.BAR_CLUSTERED,
        "column": XL_CHART_TYPE.COLUMN_CLUSTERED,
        "line": XL_CHART_TYPE.LINE,
        "pie": XL_CHART_TYPE.PIE,
        "area": XL_CHART_TYPE.AREA,
    }

    # 차트 데이터 준비
    chart_data = CategoryChartData()
    chart_data.categories = categories

    for series in series_data:
        chart_data.add_series(series["name"], series["values"])

    # 차트 추가
    chart_x = Inches(0.7)
    chart_y = Inches(content_top + 0.8)
    chart_width = Inches(12)
    chart_height = Inches(4.5)

    chart = slide.shapes.add_chart(
        chart_types.get(chart_type, XL_CHART_TYPE.COLUMN_CLUSTERED),
        chart_x, chart_y, chart_width, chart_height,
        chart_data
    ).chart

    # 차트 스타일링
    chart.has_legend = len(series_data) > 1
    if chart.has_legend:
        chart.legend.include_in_layout = False

    # 주석
    if note:
        note_box = slide.shapes.add_textbox(
            Inches(0.7), Inches(6.3),
            Inches(12), Inches(0.4)
        )
        tf = note_box.text_frame
        tf.paragraphs[0].text = f"* {note}"
        tf.paragraphs[0].font.size = Pt(10)
        tf.paragraphs[0].font.italic = True
        tf.paragraphs[0].font.color.rgb = style['text_light']

    return slide
```

## 사용 예시

```python
# 매출 성장 차트
add_chart_slide(
    prs,
    title="매출 성장 추이",
    chart_type="column",
    categories=["2022", "2023", "2024", "2025(E)"],
    series_data=[
        {"name": "매출", "values": [100, 250, 500, 1000]}
    ],
    note="2025년은 예상치",
    style=corporate_style,
    header_func=add_header,
    page_num=7
)

# 시장 점유율 파이 차트
add_chart_slide(
    prs,
    title="시장 점유율",
    chart_type="pie",
    categories=["우리", "경쟁사A", "경쟁사B", "기타"],
    series_data=[
        {"name": "점유율", "values": [35, 25, 20, 20]}
    ],
    style=corporate_style,
    ...
)
```

## 차트 타입별 권장 용도

| 타입 | 용도 |
|------|------|
| column | 기간별 비교, 성장 추이 |
| bar | 항목별 비교 (긴 라벨) |
| line | 트렌드, 시계열 데이터 |
| pie | 비율, 구성 |
| area | 누적 추이 |
