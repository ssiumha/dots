# Minimal Header

로고 + 제목만 있는 깔끔한 헤더.

## 레이아웃

```
┌─────────────────────────────────────────────────┐
│ [Logo]                          Section Title   │  ← 헤더 (높이: 0.7")
├─────────────────────────────────────────────────┤
│                                                 │
│              [Content Area]                     │  ← 콘텐츠 (6.3")
│                                                 │
├─────────────────────────────────────────────────┤
│                                    Page Number  │  ← 푸터 (0.5")
└─────────────────────────────────────────────────┘
```

## 코드

```python
def add_header(slide, section_title, style):
    """모든 슬라이드에 minimal 헤더 추가"""
    # 헤더 배경 (선택적)
    header_bg = slide.shapes.add_shape(
        MSO_SHAPE.RECTANGLE,
        Inches(0), Inches(0),
        Inches(13.333), Inches(0.7)
    )
    header_bg.fill.solid()
    header_bg.fill.fore_color.rgb = style['header_bg']
    header_bg.line.fill.background()

    # 로고 (좌측)
    if style.get('logo_path'):
        slide.shapes.add_picture(
            style['logo_path'],
            Inches(0.3), Inches(0.15),
            height=Inches(0.4)
        )

    # 섹션 제목 (우측)
    title_box = slide.shapes.add_textbox(
        Inches(10), Inches(0.2),
        Inches(3), Inches(0.4)
    )
    tf = title_box.text_frame
    tf.paragraphs[0].text = section_title
    tf.paragraphs[0].font.size = Pt(14)
    tf.paragraphs[0].font.color.rgb = style['header_text']
    tf.paragraphs[0].alignment = PP_ALIGN.RIGHT


def add_footer(slide, page_num, style):
    """페이지 번호 푸터"""
    footer = slide.shapes.add_textbox(
        Inches(12.5), Inches(7.1),
        Inches(0.5), Inches(0.3)
    )
    tf = footer.text_frame
    tf.paragraphs[0].text = str(page_num)
    tf.paragraphs[0].font.size = Pt(10)
    tf.paragraphs[0].font.color.rgb = style['footer_text']
    tf.paragraphs[0].alignment = PP_ALIGN.RIGHT
```

## 사용 예시

```python
# 슬라이드 생성 후 헤더/푸터 추가
slide = prs.slides.add_slide(blank_layout)
add_header(slide, "문제 정의", style)
add_footer(slide, 2, style)
# ... 콘텐츠 추가
```
