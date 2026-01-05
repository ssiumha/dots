# Closing Slide

마무리 슬라이드. 연락처, CTA, 감사 인사.

## 레이아웃 변형

### 연락처 중심

```
┌─────────────────────────────────────────────────┐
│                                                 │
│                                                 │
│              Thank You                          │
│                                                 │
│         contact@company.com                     │
│         +82-10-1234-5678                        │
│         www.company.com                         │
│                                                 │
│                                      [Logo]     │
└─────────────────────────────────────────────────┘
```

### CTA 중심 (Call to Action)

```
┌─────────────────────────────────────────────────┐
│                                                 │
│                                                 │
│          Ready to Start?                        │
│                                                 │
│        ┌─────────────────────┐                  │
│        │   Contact Us Now    │  ← CTA 버튼      │
│        └─────────────────────┘                  │
│                                                 │
│         contact@company.com                     │
└─────────────────────────────────────────────────┘
```

## 코드

```python
def add_closing_slide(prs, main_text, contacts, cta_text=None, style=None):
    """
    마무리 슬라이드 (헤더 없음)

    Args:
        main_text: "Thank You" 또는 "Ready to Start?"
        contacts: {
            "email": "contact@company.com",
            "phone": "+82-10-1234-5678",
            "website": "www.company.com"
        }
        cta_text: CTA 버튼 텍스트 (선택)
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
    background.fill.fore_color.rgb = style['background']
    background.line.fill.background()

    # 메인 텍스트
    main_box = slide.shapes.add_textbox(
        Inches(1), Inches(2),
        Inches(11.333), Inches(1.2)
    )
    tf = main_box.text_frame
    tf.paragraphs[0].text = main_text
    tf.paragraphs[0].font.size = Pt(44)
    tf.paragraphs[0].font.bold = True
    tf.paragraphs[0].font.color.rgb = style['title']
    tf.paragraphs[0].alignment = PP_ALIGN.CENTER

    # CTA 버튼 (선택적)
    cta_y = 3.5
    if cta_text:
        cta_button = slide.shapes.add_shape(
            MSO_SHAPE.ROUNDED_RECTANGLE,
            Inches(4.5), Inches(cta_y),
            Inches(4.333), Inches(0.8)
        )
        cta_button.fill.solid()
        cta_button.fill.fore_color.rgb = style['primary']
        cta_button.line.fill.background()

        # CTA 텍스트
        cta_text_box = slide.shapes.add_textbox(
            Inches(4.5), Inches(cta_y + 0.15),
            Inches(4.333), Inches(0.5)
        )
        tf = cta_text_box.text_frame
        tf.paragraphs[0].text = cta_text
        tf.paragraphs[0].font.size = Pt(18)
        tf.paragraphs[0].font.bold = True
        tf.paragraphs[0].font.color.rgb = style['white']
        tf.paragraphs[0].alignment = PP_ALIGN.CENTER

        cta_y += 1.5

    # 연락처 정보
    contact_y = cta_y + 0.5 if cta_text else 3.8
    contact_items = []
    if contacts.get('email'):
        contact_items.append(contacts['email'])
    if contacts.get('phone'):
        contact_items.append(contacts['phone'])
    if contacts.get('website'):
        contact_items.append(contacts['website'])

    contact_box = slide.shapes.add_textbox(
        Inches(1), Inches(contact_y),
        Inches(11.333), Inches(1.5)
    )
    tf = contact_box.text_frame

    for i, item in enumerate(contact_items):
        if i == 0:
            p = tf.paragraphs[0]
        else:
            p = tf.add_paragraph()
        p.text = item
        p.font.size = Pt(16)
        p.font.color.rgb = style['text_light']
        p.alignment = PP_ALIGN.CENTER
        p.space_before = Pt(8)

    # 로고 (우하단)
    if style.get('logo_path'):
        slide.shapes.add_picture(
            style['logo_path'],
            Inches(11), Inches(6.3),
            height=Inches(0.8)
        )

    return slide
```

## 사용 예시

```python
# 감사 인사 + 연락처
add_closing_slide(
    prs,
    main_text="감사합니다",
    contacts={
        "email": "contact@startup.com",
        "phone": "02-1234-5678",
        "website": "www.startup.com"
    },
    style=corporate_style
)

# CTA 포함
add_closing_slide(
    prs,
    main_text="함께 성장할 준비가 되셨나요?",
    contacts={
        "email": "invest@startup.com"
    },
    cta_text="지금 문의하기",
    style=startup_style
)
```
