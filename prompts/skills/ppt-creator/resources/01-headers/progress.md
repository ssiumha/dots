# Progress Header

진행률 바 포함. 단계별 프레젠테이션에 적합.

## 레이아웃

```
┌─────────────────────────────────────────────────┐
│ [Logo]                          Section Title   │
│ ●────●────●────○────○────○────○────○────○────○  │  ← 진행률 바 (0.9")
│ 1    2    3    4    5    6    7    8    9   10  │
├─────────────────────────────────────────────────┤
│                                                 │
│              [Content Area]                     │  ← 콘텐츠 (6.1")
│                                                 │
├─────────────────────────────────────────────────┤
│                                    Page Number  │  ← 푸터 (0.5")
└─────────────────────────────────────────────────┘
```

## 코드

```python
def add_progress_header(slide, section_title, current_step, total_steps, style):
    """진행률 바가 있는 헤더"""
    # 헤더 배경
    header_bg = slide.shapes.add_shape(
        MSO_SHAPE.RECTANGLE,
        Inches(0), Inches(0),
        Inches(13.333), Inches(0.9)
    )
    header_bg.fill.solid()
    header_bg.fill.fore_color.rgb = style['header_bg']
    header_bg.line.fill.background()

    # 로고
    if style.get('logo_path'):
        slide.shapes.add_picture(
            style['logo_path'],
            Inches(0.3), Inches(0.1),
            height=Inches(0.35)
        )

    # 섹션 제목
    title_box = slide.shapes.add_textbox(
        Inches(10), Inches(0.1),
        Inches(3), Inches(0.35)
    )
    tf = title_box.text_frame
    tf.paragraphs[0].text = section_title
    tf.paragraphs[0].font.size = Pt(12)
    tf.paragraphs[0].font.color.rgb = style['header_text']
    tf.paragraphs[0].alignment = PP_ALIGN.RIGHT

    # 진행률 바
    bar_start_x = 1.5
    bar_width = 10.5
    step_width = bar_width / (total_steps - 1) if total_steps > 1 else bar_width

    # 배경 라인
    line = slide.shapes.add_shape(
        MSO_SHAPE.RECTANGLE,
        Inches(bar_start_x), Inches(0.6),
        Inches(bar_width), Inches(0.03)
    )
    line.fill.solid()
    line.fill.fore_color.rgb = style['gray_light']
    line.line.fill.background()

    # 진행 라인 (현재까지)
    if current_step > 1:
        progress_width = step_width * (current_step - 1)
        progress_line = slide.shapes.add_shape(
            MSO_SHAPE.RECTANGLE,
            Inches(bar_start_x), Inches(0.6),
            Inches(progress_width), Inches(0.03)
        )
        progress_line.fill.solid()
        progress_line.fill.fore_color.rgb = style['primary']
        progress_line.line.fill.background()

    # 스텝 원형 마커
    for i in range(1, total_steps + 1):
        x = bar_start_x + step_width * (i - 1) - 0.08

        circle = slide.shapes.add_shape(
            MSO_SHAPE.OVAL,
            Inches(x), Inches(0.53),
            Inches(0.16), Inches(0.16)
        )

        if i <= current_step:
            circle.fill.solid()
            circle.fill.fore_color.rgb = style['primary']
        else:
            circle.fill.solid()
            circle.fill.fore_color.rgb = style['gray_light']
        circle.line.fill.background()


# 사용 예시
add_progress_header(slide, "솔루션", current_step=3, total_steps=10, style)
```

## 적합한 용도

- 투자 피치덱 (10단계)
- 교육/튜토리얼 자료
- 프로세스 설명
