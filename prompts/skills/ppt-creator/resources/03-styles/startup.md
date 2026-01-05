# Startup Style

스타트업 피치덱용. 그라디언트, 밝은 색감, 활기찬 느낌.

## 색상 팔레트

```
Primary:    #6C5CE7 (Purple)        ████
Secondary:  #00CEC9 (Cyan)          ████
Accent:     #FD79A8 (Pink)          ████

Text:       #2D3436 (Charcoal)      ████
Text Light: #636E72 (Gray)          ████
Background: #FFFFFF (White)         ████
Section BG: #6C5CE7 (Purple)        ████
```

## 코드

```python
from pptx.dml.color import RGBColor

startup_style = {
    # 주요 색상 (밝고 활기찬)
    "primary": RGBColor(108, 92, 231),    # #6C5CE7 Purple
    "secondary": RGBColor(0, 206, 201),   # #00CEC9 Cyan
    "accent": RGBColor(253, 121, 168),    # #FD79A8 Pink

    # 텍스트
    "title": RGBColor(45, 52, 54),        # #2D3436
    "subtitle": RGBColor(108, 92, 231),   # primary
    "text": RGBColor(45, 52, 54),         # #2D3436
    "text_light": RGBColor(99, 110, 114), # #636E72

    # 배경
    "background": RGBColor(255, 255, 255),
    "header_bg": RGBColor(255, 255, 255),
    "header_text": RGBColor(99, 110, 114),

    # 보조
    "white": RGBColor(255, 255, 255),
    "gray_light": RGBColor(223, 230, 233),  # #DFE6E9
    "gray_lightest": RGBColor(245, 246, 250),
    "footer_text": RGBColor(99, 110, 114),

    # 다이어그램용
    "arrow": RGBColor(0, 206, 201),        # secondary (cyan)
    "border": RGBColor(108, 92, 231),      # primary (purple)

    # 로고
    "logo_path": None,
}
```

## 폰트 설정

```python
startup_fonts = {
    "title": {
        "name": "Pretendard",      # 또는 "Spoqa Han Sans Neo"
        "size": Pt(32),
        "bold": True
    },
    "subtitle": {
        "name": "Pretendard",
        "size": Pt(18),
        "bold": False
    },
    "body": {
        "name": "Pretendard",
        "size": Pt(16),
        "bold": False
    },
    "caption": {
        "name": "Pretendard",
        "size": Pt(11),
        "bold": False
    }
}
```

## 적합한 용도

- 투자 피치덱
- 제품 소개
- 데모 발표
- 스타트업 IR
- 창업 경진대회

## 그라디언트 배경 (고급)

```python
from pptx.oxml.ns import qn
from pptx.oxml import parse_xml

def add_gradient_background(slide, color1, color2):
    """섹션 슬라이드용 그라디언트 배경"""
    # python-pptx는 그라디언트 직접 지원 안함
    # 대안: 단색 + 투명도 도형 레이어링

    # 방법 1: 단색 배경
    bg = slide.shapes.add_shape(
        MSO_SHAPE.RECTANGLE,
        Inches(0), Inches(0),
        Inches(13.333), Inches(7.5)
    )
    bg.fill.solid()
    bg.fill.fore_color.rgb = color1
    bg.line.fill.background()

    # 방법 2: 기존 그라디언트 이미지 사용
    # slide.shapes.add_picture("gradient_bg.png", ...)
```

## 샘플 조합

```python
# Startup + Progress Header (10단계 피치덱)
prs = create_presentation()

add_title_slide(prs, "OO 서비스", "The Future of ...", style=startup_style)
add_section_slide(prs, 1, "Problem", style=startup_style)
add_content_slide(prs, "시장의 문제", [...], header_func=add_progress_header, style=startup_style)
add_image_text_slide(prs, "Our Solution", "product.png", [...], style=startup_style)
add_chart_slide(prs, "Market Size", "pie", [...], style=startup_style)
add_closing_slide(prs, "Let's Build Together", {...}, cta_text="Invest Now", style=startup_style)
```
