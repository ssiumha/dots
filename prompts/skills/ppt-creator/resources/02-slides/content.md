# Content Slide

제목 + 불릿 포인트. 가장 기본적인 콘텐츠 슬라이드.

## 레이아웃

```
┌─────────────────────────────────────────────────┐
│ [Header]                                        │  ← 헤더 (0.7"-0.9")
├─────────────────────────────────────────────────┤
│                                                 │
│   Slide Title                                   │  ← 제목 (1")
│                                                 │
│   • Bullet point 1                              │
│   • Bullet point 2                              │  ← 본문 (4.5")
│   • Bullet point 3                              │
│     - Sub bullet                                │
│                                                 │
├─────────────────────────────────────────────────┤
│ [Footer]                                        │  ← 푸터 (0.5")
└─────────────────────────────────────────────────┘
```

## 코드

```python
def add_content_slide(prs, title, bullets, sub_bullets=None, style=None, header_func=None, page_num=1):
    """
    제목 + 불릿 포인트 슬라이드

    Args:
        bullets: ["항목1", "항목2", ...]
        sub_bullets: {1: ["서브1", "서브2"]} - 인덱스별 서브 불릿
    """
    blank_layout = prs.slide_layouts[6]
    slide = prs.slides.add_slide(blank_layout)

    # 헤더/푸터 추가
    if header_func:
        header_func(slide, title, style)
    add_footer(slide, page_num, style)

    # 콘텐츠 영역 시작 Y (헤더 아래)
    content_top = 1.0

    # 슬라이드 제목
    title_box = slide.shapes.add_textbox(
        Inches(0.7), Inches(content_top),
        Inches(12), Inches(0.8)
    )
    tf = title_box.text_frame
    tf.paragraphs[0].text = title
    tf.paragraphs[0].font.size = Pt(28)
    tf.paragraphs[0].font.bold = True
    tf.paragraphs[0].font.color.rgb = style['title']

    # 불릿 포인트
    bullet_box = slide.shapes.add_textbox(
        Inches(0.7), Inches(content_top + 1),
        Inches(12), Inches(5)
    )
    tf = bullet_box.text_frame
    tf.word_wrap = True

    sub_bullets = sub_bullets or {}

    for i, bullet in enumerate(bullets):
        if i == 0:
            p = tf.paragraphs[0]
        else:
            p = tf.add_paragraph()

        p.text = bullet
        p.font.size = Pt(18)
        p.font.color.rgb = style['text']
        p.level = 0
        p.space_before = Pt(12)

        # 서브 불릿 추가
        if i in sub_bullets:
            for sub in sub_bullets[i]:
                sp = tf.add_paragraph()
                sp.text = sub
                sp.font.size = Pt(14)
                sp.font.color.rgb = style['text_light']
                sp.level = 1
                sp.space_before = Pt(6)

    return slide
```

## 사용 예시

```python
add_content_slide(
    prs,
    title="문제 정의",
    bullets=[
        "현재 시장의 주요 문제점",
        "기존 솔루션의 한계",
        "고객이 겪는 불편함"
    ],
    sub_bullets={
        1: ["높은 비용", "복잡한 사용법"],
        2: ["긴 대기 시간", "낮은 만족도"]
    },
    style=corporate_style,
    header_func=add_header,
    page_num=2
)
```

## 불릿 개수 가이드

- 최대 5개 권장
- 서브 불릿은 2-3개 이내
- 텍스트는 한 줄 권장 (50자 이내)
