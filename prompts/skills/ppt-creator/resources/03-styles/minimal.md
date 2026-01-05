# Minimal Style

미니멀 스타일. 흑백 기반, 깔끔하고 모던한 느낌.

## 색상 팔레트

```
Primary:    #000000 (Black)         ████
Secondary:  #333333 (Dark Gray)     ████
Accent:     #FF4757 (Red)           ████  ← 강조용만

Text:       #1A1A1A (Near Black)    ████
Text Light: #666666 (Gray)          ████
Background: #FFFFFF (White)         ████
Header BG:  #FAFAFA (Off White)     ████
```

## 코드

```python
from pptx.dml.color import RGBColor

minimal_style = {
    # 주요 색상 (모노톤)
    "primary": RGBColor(0, 0, 0),          # Black
    "secondary": RGBColor(51, 51, 51),     # #333333
    "accent": RGBColor(255, 71, 87),       # #FF4757 (강조용)

    # 텍스트
    "title": RGBColor(26, 26, 26),         # #1A1A1A
    "subtitle": RGBColor(102, 102, 102),   # #666666
    "text": RGBColor(26, 26, 26),          # #1A1A1A
    "text_light": RGBColor(102, 102, 102), # #666666

    # 배경
    "background": RGBColor(255, 255, 255),
    "header_bg": RGBColor(250, 250, 250),  # #FAFAFA
    "header_text": RGBColor(102, 102, 102),

    # 보조
    "white": RGBColor(255, 255, 255),
    "gray_light": RGBColor(238, 238, 238),  # #EEEEEE
    "gray_lightest": RGBColor(250, 250, 250),
    "footer_text": RGBColor(153, 153, 153),

    # 로고
    "logo_path": None,
}
```

## 폰트 설정

```python
minimal_fonts = {
    "title": {
        "name": "Apple SD Gothic Neo",  # 또는 "Helvetica Neue", "SF Pro"
        "size": Pt(28),
        "bold": True
    },
    "subtitle": {
        "name": "Apple SD Gothic Neo",
        "size": Pt(16),
        "bold": False
    },
    "body": {
        "name": "Apple SD Gothic Neo",
        "size": Pt(14),
        "bold": False
    },
    "caption": {
        "name": "Apple SD Gothic Neo",
        "size": Pt(10),
        "bold": False
    }
}
```

## 적합한 용도

- 디자인 포트폴리오
- 크리에이티브 제안
- 제품 런칭
- 브랜드 가이드라인
- 미니멀 기업 소개

## 디자인 원칙

1. **여백**: 일반보다 20% 더 여백
2. **타이포**: 큰 제목, 적은 본문
3. **색상**: 흑백 + 포인트 컬러 1개만
4. **이미지**: 고품질 사진, 풀블리드 가능
5. **정보**: 슬라이드당 요소 3개 이하

## 샘플 조합

```python
# Minimal + Minimal Header
prs = create_presentation()

add_title_slide(prs, "BRAND", "Designed for Simplicity", style=minimal_style)
add_section_slide(prs, None, "CONCEPT", style=minimal_style)  # 번호 없음
add_image_text_slide(prs, "", "hero_image.jpg", [...], style=minimal_style)
add_content_slide(prs, "KEY POINTS", ["One.", "Two.", "Three."], style=minimal_style)
add_closing_slide(prs, "CONTACT", {...}, style=minimal_style)
```

## 풀블리드 이미지 슬라이드

```python
def add_fullbleed_image_slide(prs, image_path, overlay_text="", style=None):
    """전체 화면 이미지 + 오버레이 텍스트"""
    blank_layout = prs.slide_layouts[6]
    slide = prs.slides.add_slide(blank_layout)

    # 전체 화면 이미지
    slide.shapes.add_picture(
        image_path,
        Inches(0), Inches(0),
        width=Inches(13.333), height=Inches(7.5)
    )

    # 반투명 오버레이 (텍스트 가독성용)
    if overlay_text:
        overlay = slide.shapes.add_shape(
            MSO_SHAPE.RECTANGLE,
            Inches(0), Inches(5),
            Inches(13.333), Inches(2.5)
        )
        overlay.fill.solid()
        overlay.fill.fore_color.rgb = RGBColor(0, 0, 0)
        # 투명도는 XML 조작 필요

        text_box = slide.shapes.add_textbox(
            Inches(0.7), Inches(5.5),
            Inches(12), Inches(1.5)
        )
        tf = text_box.text_frame
        tf.paragraphs[0].text = overlay_text
        tf.paragraphs[0].font.size = Pt(36)
        tf.paragraphs[0].font.bold = True
        tf.paragraphs[0].font.color.rgb = style['white']

    return slide
```
