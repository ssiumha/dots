# Corporate Style

기업 제안서, 공식 문서용. 파랑/회색 기반 안정감.

## 색상 팔레트

```
Primary:    #1E3A5F (Navy Blue)     ████
Secondary:  #4A90A4 (Steel Blue)    ████
Accent:     #E67E22 (Orange)        ████

Text:       #2C3E50 (Dark Gray)     ████
Text Light: #7F8C8D (Gray)          ████
Background: #FFFFFF (White)         ████
Header BG:  #F8F9FA (Light Gray)    ████
```

## 코드

```python
from pptx.dml.color import RGBColor

corporate_style = {
    # 주요 색상
    "primary": RGBColor(30, 58, 95),      # #1E3A5F
    "secondary": RGBColor(74, 144, 164),  # #4A90A4
    "accent": RGBColor(230, 126, 34),     # #E67E22

    # 텍스트
    "title": RGBColor(30, 58, 95),        # primary와 동일
    "subtitle": RGBColor(74, 144, 164),   # secondary
    "text": RGBColor(44, 62, 80),         # #2C3E50
    "text_light": RGBColor(127, 140, 141),# #7F8C8D

    # 배경
    "background": RGBColor(255, 255, 255),
    "header_bg": RGBColor(248, 249, 250),  # #F8F9FA
    "header_text": RGBColor(44, 62, 80),

    # 보조
    "white": RGBColor(255, 255, 255),
    "gray_light": RGBColor(220, 220, 220),
    "gray_lightest": RGBColor(245, 245, 245),
    "footer_text": RGBColor(127, 140, 141),

    # 다이어그램용
    "arrow": RGBColor(74, 144, 164),      # secondary
    "border": RGBColor(30, 58, 95),       # primary

    # 로고 (선택)
    "logo_path": None,  # "./assets/logo.png"
}
```

## 폰트 설정

```python
corporate_fonts = {
    "title": {
        "name": "맑은 고딕",       # 또는 "Pretendard", "Noto Sans KR"
        "size": Pt(28),
        "bold": True
    },
    "subtitle": {
        "name": "맑은 고딕",
        "size": Pt(18),
        "bold": False
    },
    "body": {
        "name": "맑은 고딕",
        "size": Pt(14),
        "bold": False
    },
    "caption": {
        "name": "맑은 고딕",
        "size": Pt(10),
        "bold": False
    }
}
```

## 적합한 용도

- 기업 제안서
- 투자 보고서
- 공식 프레젠테이션
- 컨설팅 자료
- IR 자료

## 샘플 조합

```python
# Corporate + Minimal Header
prs = create_presentation()

add_title_slide(prs, "2025년 사업계획", "주식회사 OO", style=corporate_style)
add_section_slide(prs, 1, "현황 분석", style=corporate_style)
add_content_slide(prs, "주요 성과", [...], header_func=add_header, style=corporate_style)
add_chart_slide(prs, "매출 추이", "column", [...], style=corporate_style)
add_table_slide(prs, "재무 계획", [...], style=corporate_style)
add_closing_slide(prs, "감사합니다", {...}, style=corporate_style)
```
