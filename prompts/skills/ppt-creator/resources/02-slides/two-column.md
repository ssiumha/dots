# Two-Column Slide

2단 레이아웃. 비교, Before/After, 좌우 대비에 적합.

## 레이아웃

```
┌─────────────────────────────────────────────────┐
│ [Header]                                        │
├─────────────────────────────────────────────────┤
│   Slide Title                                   │
├───────────────────────┬─────────────────────────┤
│                       │                         │
│   Left Column         │    Right Column         │
│   • Item 1            │    • Item A             │
│   • Item 2            │    • Item B             │
│                       │                         │
├───────────────────────┴─────────────────────────┤
│ [Footer]                                        │
└─────────────────────────────────────────────────┘
```

## 코드

```python
def add_two_column_slide(prs, title, left_title, left_items, right_title, right_items,
                         style=None, header_func=None, page_num=1):
    """
    2단 비교 슬라이드

    Args:
        left_title: 좌측 컬럼 제목
        left_items: ["항목1", "항목2", ...]
        right_title: 우측 컬럼 제목
        right_items: ["항목A", "항목B", ...]
    """
    blank_layout = prs.slide_layouts[6]
    slide = prs.slides.add_slide(blank_layout)

    if header_func:
        header_func(slide, title, style)
    add_footer(slide, page_num, style)

    content_top = 1.0

    # 메인 제목
    title_box = slide.shapes.add_textbox(
        Inches(0.7), Inches(content_top),
        Inches(12), Inches(0.7)
    )
    tf = title_box.text_frame
    tf.paragraphs[0].text = title
    tf.paragraphs[0].font.size = Pt(28)
    tf.paragraphs[0].font.bold = True
    tf.paragraphs[0].font.color.rgb = style['title']

    # 구분선 (선택적)
    divider = slide.shapes.add_shape(
        MSO_SHAPE.RECTANGLE,
        Inches(6.6), Inches(content_top + 0.9),
        Inches(0.02), Inches(5)
    )
    divider.fill.solid()
    divider.fill.fore_color.rgb = style['gray_light']
    divider.line.fill.background()

    # 좌측 컬럼
    _add_column(slide,
                x=0.7, y=content_top + 0.9, width=5.5,
                col_title=left_title, items=left_items, style=style)

    # 우측 컬럼
    _add_column(slide,
                x=7.0, y=content_top + 0.9, width=5.5,
                col_title=right_title, items=right_items, style=style)

    return slide


def _add_column(slide, x, y, width, col_title, items, style):
    """컬럼 내용 추가 헬퍼"""
    # 컬럼 제목
    title_box = slide.shapes.add_textbox(
        Inches(x), Inches(y),
        Inches(width), Inches(0.5)
    )
    tf = title_box.text_frame
    tf.paragraphs[0].text = col_title
    tf.paragraphs[0].font.size = Pt(18)
    tf.paragraphs[0].font.bold = True
    tf.paragraphs[0].font.color.rgb = style['primary']

    # 컬럼 항목들
    items_box = slide.shapes.add_textbox(
        Inches(x), Inches(y + 0.6),
        Inches(width), Inches(4)
    )
    tf = items_box.text_frame
    tf.word_wrap = True

    for i, item in enumerate(items):
        if i == 0:
            p = tf.paragraphs[0]
        else:
            p = tf.add_paragraph()
        p.text = item
        p.font.size = Pt(14)
        p.font.color.rgb = style['text']
        p.space_before = Pt(10)
```

## 사용 예시

```python
# AS-IS / TO-BE 비교
add_two_column_slide(
    prs,
    title="현황 vs 목표",
    left_title="AS-IS",
    left_items=["수동 프로세스", "3일 소요", "오류율 15%"],
    right_title="TO-BE",
    right_items=["자동화", "실시간 처리", "오류율 1% 미만"],
    style=corporate_style,
    header_func=add_header,
    page_num=5
)

# 경쟁사 비교
add_two_column_slide(
    prs,
    title="경쟁 우위",
    left_title="경쟁사",
    left_items=["높은 가격", "복잡한 UI", "제한된 기능"],
    right_title="우리 서비스",
    right_items=["합리적 가격", "직관적 UI", "확장 가능"],
    ...
)
```
