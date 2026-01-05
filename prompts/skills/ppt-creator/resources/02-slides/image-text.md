# Image-Text Slide

이미지 + 텍스트 조합. 제품 스크린샷, 다이어그램 설명에 적합.

## 레이아웃 변형

### 이미지 좌측

```
┌─────────────────────────────────────────────────┐
│ [Header]                                        │
├─────────────────────────────────────────────────┤
│   Slide Title                                   │
├────────────────────┬────────────────────────────┤
│                    │                            │
│    [Image]         │   Text content             │
│                    │   • Bullet 1               │
│                    │   • Bullet 2               │
│                    │                            │
├────────────────────┴────────────────────────────┤
│ [Footer]                                        │
└─────────────────────────────────────────────────┘
```

### 이미지 우측

```
┌─────────────────────────────────────────────────┐
│ [Header]                                        │
├─────────────────────────────────────────────────┤
│   Slide Title                                   │
├────────────────────────────┬────────────────────┤
│                            │                    │
│   Text content             │    [Image]         │
│   • Bullet 1               │                    │
│   • Bullet 2               │                    │
│                            │                    │
├────────────────────────────┴────────────────────┤
│ [Footer]                                        │
└─────────────────────────────────────────────────┘
```

## 코드

```python
def add_image_text_slide(prs, title, image_path, text_title, text_items,
                         image_position="left", style=None, header_func=None, page_num=1):
    """
    이미지 + 텍스트 슬라이드

    Args:
        image_path: 이미지 파일 경로
        text_title: 텍스트 영역 제목
        text_items: ["설명1", "설명2", ...]
        image_position: "left" 또는 "right"
    """
    blank_layout = prs.slide_layouts[6]
    slide = prs.slides.add_slide(blank_layout)

    if header_func:
        header_func(slide, title, style)
    add_footer(slide, page_num, style)

    content_top = 1.0

    # 메인 제목
    title_box = slide.shapes.add_textbox(
        Inches(0.7), Inches(content_top),
        Inches(12), Inches(0.7)
    )
    tf = title_box.text_frame
    tf.paragraphs[0].text = title
    tf.paragraphs[0].font.size = Pt(28)
    tf.paragraphs[0].font.bold = True
    tf.paragraphs[0].font.color.rgb = style['title']

    # 위치에 따른 좌표 설정
    if image_position == "left":
        img_x, img_width = 0.7, 5.5
        text_x, text_width = 6.5, 6
    else:  # right
        img_x, img_width = 6.8, 5.8
        text_x, text_width = 0.7, 5.8

    # 이미지 추가
    img_y = content_top + 0.9
    img_height = 4.5

    slide.shapes.add_picture(
        image_path,
        Inches(img_x), Inches(img_y),
        width=Inches(img_width)
        # height는 자동 비율 조정
    )

    # 텍스트 영역
    # 서브 제목
    sub_title_box = slide.shapes.add_textbox(
        Inches(text_x), Inches(img_y),
        Inches(text_width), Inches(0.5)
    )
    tf = sub_title_box.text_frame
    tf.paragraphs[0].text = text_title
    tf.paragraphs[0].font.size = Pt(18)
    tf.paragraphs[0].font.bold = True
    tf.paragraphs[0].font.color.rgb = style['primary']

    # 텍스트 항목들
    items_box = slide.shapes.add_textbox(
        Inches(text_x), Inches(img_y + 0.7),
        Inches(text_width), Inches(4)
    )
    tf = items_box.text_frame
    tf.word_wrap = True

    for i, item in enumerate(text_items):
        if i == 0:
            p = tf.paragraphs[0]
        else:
            p = tf.add_paragraph()
        p.text = item
        p.font.size = Pt(14)
        p.font.color.rgb = style['text']
        p.space_before = Pt(10)

    return slide
```

## 사용 예시

```python
# 제품 스크린샷 + 설명
add_image_text_slide(
    prs,
    title="제품 데모",
    image_path="./screenshots/dashboard.png",
    text_title="주요 기능",
    text_items=[
        "실시간 대시보드",
        "원클릭 리포트 생성",
        "팀 협업 기능",
        "모바일 지원"
    ],
    image_position="left",
    style=corporate_style,
    header_func=add_header,
    page_num=6
)

# 아키텍처 다이어그램
add_image_text_slide(
    prs,
    title="시스템 아키텍처",
    image_path="./diagrams/architecture.png",
    text_title="구성 요소",
    text_items=[
        "마이크로서비스 기반",
        "Kubernetes 오케스트레이션",
        "실시간 데이터 파이프라인"
    ],
    image_position="right",
    ...
)
```

## 이미지 권장 사항

- 해상도: 1920x1080 이상
- 비율: 16:9 또는 4:3
- 형식: PNG (투명 배경) 권장
