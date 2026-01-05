# Table Slide

테이블 슬라이드. 비교표, 일정, 재무 데이터에 적합.

## 레이아웃

```
┌─────────────────────────────────────────────────┐
│ [Header]                                        │
├─────────────────────────────────────────────────┤
│   Table Title                                   │
│ ┌──────────┬──────────┬──────────┬──────────┐  │
│ │  Header  │  Header  │  Header  │  Header  │  │
│ ├──────────┼──────────┼──────────┼──────────┤  │
│ │  Cell    │  Cell    │  Cell    │  Cell    │  │
│ │  Cell    │  Cell    │  Cell    │  Cell    │  │
│ │  Cell    │  Cell    │  Cell    │  Cell    │  │
│ └──────────┴──────────┴──────────┴──────────┘  │
├─────────────────────────────────────────────────┤
│ [Footer]                                        │
└─────────────────────────────────────────────────┘
```

## 코드

```python
def add_table_slide(prs, title, headers, rows, style=None, header_func=None, page_num=1):
    """
    테이블 슬라이드

    Args:
        headers: ["항목", "Q1", "Q2", "Q3", "Q4"]
        rows: [
            ["매출", "100", "150", "200", "250"],
            ["비용", "80", "100", "120", "140"],
            ["이익", "20", "50", "80", "110"]
        ]
    """
    blank_layout = prs.slide_layouts[6]
    slide = prs.slides.add_slide(blank_layout)

    if header_func:
        header_func(slide, title, style)
    add_footer(slide, page_num, style)

    content_top = 1.0

    # 슬라이드 제목
    title_box = slide.shapes.add_textbox(
        Inches(0.7), Inches(content_top),
        Inches(12), Inches(0.6)
    )
    tf = title_box.text_frame
    tf.paragraphs[0].text = title
    tf.paragraphs[0].font.size = Pt(28)
    tf.paragraphs[0].font.bold = True
    tf.paragraphs[0].font.color.rgb = style['title']

    # 테이블 크기 계산
    num_cols = len(headers)
    num_rows = len(rows) + 1  # 헤더 포함

    table_x = Inches(0.7)
    table_y = Inches(content_top + 0.9)
    table_width = Inches(12)
    table_height = Inches(min(num_rows * 0.6, 5))  # 최대 5인치

    # 테이블 생성
    table = slide.shapes.add_table(
        num_rows, num_cols,
        table_x, table_y, table_width, table_height
    ).table

    # 컬럼 너비 균등 분배
    col_width = int(table_width / num_cols)
    for col in table.columns:
        col.width = col_width

    # 헤더 행
    for i, header in enumerate(headers):
        cell = table.cell(0, i)
        cell.text = header
        cell.fill.solid()
        cell.fill.fore_color.rgb = style['primary']

        para = cell.text_frame.paragraphs[0]
        para.font.size = Pt(12)
        para.font.bold = True
        para.font.color.rgb = style['white']
        para.alignment = PP_ALIGN.CENTER

    # 데이터 행
    for row_idx, row in enumerate(rows):
        for col_idx, cell_text in enumerate(row):
            cell = table.cell(row_idx + 1, col_idx)
            cell.text = str(cell_text)

            # 짝수 행 배경색 (줄무늬 효과)
            if row_idx % 2 == 1:
                cell.fill.solid()
                cell.fill.fore_color.rgb = style.get('gray_lightest', RGBColor(245, 245, 245))

            para = cell.text_frame.paragraphs[0]
            para.font.size = Pt(11)
            para.font.color.rgb = style['text']

            # 첫 번째 컬럼은 좌측 정렬, 나머지는 중앙
            para.alignment = PP_ALIGN.LEFT if col_idx == 0 else PP_ALIGN.CENTER

    return slide
```

## 사용 예시

```python
# 재무 테이블
add_table_slide(
    prs,
    title="재무 계획",
    headers=["항목", "2024", "2025", "2026"],
    rows=[
        ["매출", "10억", "30억", "100억"],
        ["비용", "8억", "20억", "60억"],
        ["영업이익", "2억", "10억", "40억"],
        ["영업이익률", "20%", "33%", "40%"]
    ],
    style=corporate_style,
    header_func=add_header,
    page_num=8
)

# 일정 테이블
add_table_slide(
    prs,
    title="프로젝트 일정",
    headers=["단계", "기간", "담당", "산출물"],
    rows=[
        ["기획", "1월", "기획팀", "PRD"],
        ["설계", "2월", "개발팀", "설계서"],
        ["개발", "3-4월", "개발팀", "MVP"],
        ["테스트", "5월", "QA팀", "테스트 리포트"],
        ["출시", "6월", "전체", "서비스 오픈"]
    ],
    ...
)
```

## 테이블 가이드

- 컬럼: 최대 6개 권장
- 행: 최대 8개 권장 (가독성)
- 셀 내용: 간결하게 (15자 이내)
