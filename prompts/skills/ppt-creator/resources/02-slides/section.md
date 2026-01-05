# Section Header Slide

섹션 구분용. 큰 숫자/제목으로 새 섹션 시작 알림.

## 레이아웃

```
┌─────────────────────────────────────────────────┐
│                                                 │
│                                                 │
│   01                                            │  ← 섹션 번호 (선택)
│                                                 │
│   SECTION TITLE                                 │  ← 중앙
│   Optional subtitle                             │
│                                                 │
│                                                 │
└─────────────────────────────────────────────────┘
```

## 코드

```python
def add_section_slide(prs, section_num, section_title, subtitle="", style=None):
    """
    섹션 구분 슬라이드 (헤더 없음, 전체 화면)

    Args:
        section_num: 섹션 번호 (1, 2, 3...) 또는 None
        section_title: 섹션 제목
        subtitle: 부제목 (선택)
    """
    blank_layout = prs.slide_layouts[6]
    slide = prs.slides.add_slide(blank_layout)

    # 배경색
    background = slide.shapes.add_shape(
        MSO_SHAPE.RECTANGLE,
        Inches(0), Inches(0),
        Inches(13.333), Inches(7.5)
    )
    background.fill.solid()
    background.fill.fore_color.rgb = style['primary']
    background.line.fill.background()

    # 섹션 번호 (대형)
    if section_num:
        num_box = slide.shapes.add_textbox(
            Inches(0.7), Inches(2),
            Inches(12), Inches(1.5)
        )
        tf = num_box.text_frame
        tf.paragraphs[0].text = f"{section_num:02d}" if isinstance(section_num, int) else section_num
        tf.paragraphs[0].font.size = Pt(72)
        tf.paragraphs[0].font.bold = True
        tf.paragraphs[0].font.color.rgb = style['white']
        tf.paragraphs[0].alignment = PP_ALIGN.LEFT

    # 섹션 제목
    title_y = 3.5 if section_num else 3
    title_box = slide.shapes.add_textbox(
        Inches(0.7), Inches(title_y),
        Inches(12), Inches(1)
    )
    tf = title_box.text_frame
    tf.paragraphs[0].text = section_title
    tf.paragraphs[0].font.size = Pt(40)
    tf.paragraphs[0].font.bold = True
    tf.paragraphs[0].font.color.rgb = style['white']

    # 부제목
    if subtitle:
        sub_box = slide.shapes.add_textbox(
            Inches(0.7), Inches(title_y + 1.2),
            Inches(12), Inches(0.6)
        )
        tf = sub_box.text_frame
        tf.paragraphs[0].text = subtitle
        tf.paragraphs[0].font.size = Pt(18)
        tf.paragraphs[0].font.color.rgb = RGBColor(255, 255, 255)  # 반투명 효과

    return slide
```

## 사용 예시

```python
# 번호 있는 섹션
add_section_slide(prs, 1, "문제 정의", "Why we started", style=corporate_style)
add_section_slide(prs, 2, "솔루션", "What we built", style=corporate_style)

# 번호 없는 섹션
add_section_slide(prs, None, "Q&A", style=corporate_style)
```
