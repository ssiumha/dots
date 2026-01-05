# Base Imports

모든 슬라이드 코드에서 사용하는 공통 import.

## 필수 Import

```python
from pptx import Presentation
from pptx.util import Inches, Pt, Emu
from pptx.dml.color import RGBColor
from pptx.enum.text import PP_ALIGN, MSO_ANCHOR
from pptx.enum.shapes import MSO_SHAPE, MSO_CONNECTOR
from pptx.enum.dml import MSO_LINE_DASH_STYLE
```

## 차트용 Import

```python
from pptx.chart.data import CategoryChartData, ChartData
from pptx.enum.chart import XL_CHART_TYPE, XL_LEGEND_POSITION
```

## 테이블용 Import

```python
from pptx.table import Table
```

## 전체 템플릿

```python
#!/usr/bin/env python3
"""PPT Generator Script"""

from pptx import Presentation
from pptx.util import Inches, Pt, Emu
from pptx.dml.color import RGBColor
from pptx.enum.text import PP_ALIGN, MSO_ANCHOR
from pptx.enum.shapes import MSO_SHAPE, MSO_CONNECTOR
from pptx.enum.dml import MSO_LINE_DASH_STYLE
from pptx.chart.data import CategoryChartData
from pptx.enum.chart import XL_CHART_TYPE

# === Style Constants ===
# (03-styles에서 복사)

# === Helper Functions ===
# (01-headers, 02-slides에서 복사)

# === Main ===
def main():
    prs = Presentation()
    prs.slide_width = Inches(13.333)
    prs.slide_height = Inches(7.5)

    # 슬라이드 추가...

    prs.save("output.pptx")
    print("Generated: output.pptx")

if __name__ == "__main__":
    main()
```

## 이미지 처리 헬퍼

```python
import os

def safe_add_picture(slide, image_path, x, y, width=None, height=None):
    """이미지 안전 삽입 (파일 없으면 placeholder)"""
    if os.path.exists(image_path):
        return slide.shapes.add_picture(
            image_path, x, y,
            width=width, height=height
        )
    else:
        # Placeholder 텍스트박스
        placeholder = slide.shapes.add_textbox(x, y, width or Inches(2), height or Inches(1))
        tf = placeholder.text_frame
        tf.paragraphs[0].text = f"[Image: {os.path.basename(image_path)}]"
        tf.paragraphs[0].font.size = Pt(10)
        tf.paragraphs[0].font.italic = True
        return placeholder
```
