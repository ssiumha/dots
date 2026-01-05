# Navigation Header

상단 탭 네비게이션. 현재 섹션 하이라이트.

## 레이아웃

```
┌─────────────────────────────────────────────────┐
│ [Logo]  │ Intro │ Problem │ Solution │ Team │   │  ← 탭 네비 (0.8")
│                    ▲ 현재 섹션 하이라이트         │
├─────────────────────────────────────────────────┤
│                                                 │
│              [Content Area]                     │  ← 콘텐츠 (6.2")
│                                                 │
├─────────────────────────────────────────────────┤
│                                    Page Number  │  ← 푸터 (0.5")
└─────────────────────────────────────────────────┘
```

## 코드

```python
def add_nav_header(slide, sections, current_section, style):
    """탭 네비게이션 헤더"""
    # 헤더 배경
    header_bg = slide.shapes.add_shape(
        MSO_SHAPE.RECTANGLE,
        Inches(0), Inches(0),
        Inches(13.333), Inches(0.8)
    )
    header_bg.fill.solid()
    header_bg.fill.fore_color.rgb = style['header_bg']
    header_bg.line.fill.background()

    # 로고
    if style.get('logo_path'):
        slide.shapes.add_picture(
            style['logo_path'],
            Inches(0.3), Inches(0.2),
            height=Inches(0.4)
        )

    # 탭 네비게이션
    tab_start_x = 2.0
    tab_width = 2.0

    for i, section in enumerate(sections):
        is_current = (section == current_section)

        # 탭 배경 (현재 섹션은 하이라이트)
        tab_bg = slide.shapes.add_shape(
            MSO_SHAPE.RECTANGLE,
            Inches(tab_start_x + i * tab_width), Inches(0.15),
            Inches(tab_width - 0.1), Inches(0.5)
        )
        if is_current:
            tab_bg.fill.solid()
            tab_bg.fill.fore_color.rgb = style['primary']
        else:
            tab_bg.fill.background()
        tab_bg.line.fill.background()

        # 탭 텍스트
        tab_text = slide.shapes.add_textbox(
            Inches(tab_start_x + i * tab_width), Inches(0.25),
            Inches(tab_width - 0.1), Inches(0.3)
        )
        tf = tab_text.text_frame
        tf.paragraphs[0].text = section
        tf.paragraphs[0].font.size = Pt(11)
        tf.paragraphs[0].font.bold = is_current
        tf.paragraphs[0].font.color.rgb = (
            style['white'] if is_current else style['header_text']
        )
        tf.paragraphs[0].alignment = PP_ALIGN.CENTER


# 섹션 정의
SECTIONS = ["Intro", "Problem", "Solution", "Market", "Team"]

# 사용
add_nav_header(slide, SECTIONS, "Problem", style)
```

## 사용 시 주의

- 섹션 수는 5개 이하 권장 (공간 제약)
- 섹션명은 짧게 (영문 10자, 한글 5자 이내)
