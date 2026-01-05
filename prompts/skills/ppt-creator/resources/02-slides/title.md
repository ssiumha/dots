# Title Slide

표지 슬라이드. 헤더 없이 전체 화면 사용.

## 레이아웃

```
┌─────────────────────────────────────────────────┐
│                                                 │
│                                                 │
│                   MAIN TITLE                    │  ← 중앙 상단 (40%)
│                    Subtitle                     │
│                                                 │
│                                                 │
│                                      [Logo]     │
│   Author / Date                      Company    │  ← 하단 (20%)
└─────────────────────────────────────────────────┘
```

## 코드

```python
def add_title_slide(prs, title, subtitle, author="", date="", style=None):
    """표지 슬라이드 생성 (헤더 없음)"""
    blank_layout = prs.slide_layouts[6]  # blank
    slide = prs.slides.add_slide(blank_layout)

    # 배경색 (선택적)
    background = slide.shapes.add_shape(
        MSO_SHAPE.RECTANGLE,
        Inches(0), Inches(0),
        Inches(13.333), Inches(7.5)
    )
    background.fill.solid()
    background.fill.fore_color.rgb = style['background']
    background.line.fill.background()

    # 메인 제목
    title_box = slide.shapes.add_textbox(
        Inches(1), Inches(2.5),
        Inches(11.333), Inches(1.5)
    )
    tf = title_box.text_frame
    tf.paragraphs[0].text = title
    tf.paragraphs[0].font.size = Pt(44)
    tf.paragraphs[0].font.bold = True
    tf.paragraphs[0].font.color.rgb = style['title']
    tf.paragraphs[0].alignment = PP_ALIGN.CENTER

    # 부제목
    if subtitle:
        subtitle_box = slide.shapes.add_textbox(
            Inches(1), Inches(4),
            Inches(11.333), Inches(0.8)
        )
        tf = subtitle_box.text_frame
        tf.paragraphs[0].text = subtitle
        tf.paragraphs[0].font.size = Pt(20)
        tf.paragraphs[0].font.color.rgb = style['subtitle']
        tf.paragraphs[0].alignment = PP_ALIGN.CENTER

    # 작성자 / 날짜 (좌하단)
    if author or date:
        info_text = f"{author}  |  {date}" if author and date else (author or date)
        info_box = slide.shapes.add_textbox(
            Inches(0.5), Inches(6.8),
            Inches(5), Inches(0.4)
        )
        tf = info_box.text_frame
        tf.paragraphs[0].text = info_text
        tf.paragraphs[0].font.size = Pt(12)
        tf.paragraphs[0].font.color.rgb = style['text_light']

    # 로고 (우하단)
    if style.get('logo_path'):
        slide.shapes.add_picture(
            style['logo_path'],
            Inches(11.5), Inches(6.5),
            height=Inches(0.6)
        )

    return slide
```

## 사용 예시

```python
add_title_slide(
    prs,
    title="투자 제안서",
    subtitle="Series A - 2025년 1월",
    author="홍길동",
    date="2025.01.15",
    style=corporate_style
)
```
