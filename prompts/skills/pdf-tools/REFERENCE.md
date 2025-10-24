# PDF Tools - Technical Reference

## Architecture

**변환 파이프라인:**
1. PyMuPDF로 PDF 읽기
2. PyMuPDF4LLM으로 레이아웃 기반 Markdown 변환
3. camelot-py/pdfplumber로 표 추출
4. 이미지를 `<basename>_images/` 폴더에 저장
5. 통합 Markdown 파일 생성

**Fallback:** PyMuPDF4LLM 실패 → pdfplumber 전환

---

## Python Dependencies (구현 예정)

```toml
[project]
name = "pdf-tools"
version = "0.1.0"
dependencies = [
    "pymupdf",        # PDF 읽기 및 이미지 추출
    "pymupdf4llm",    # LLM 친화적 Markdown 변환
    "pdfplumber",     # Fallback 텍스트/표 추출
    "camelot-py[cv]", # 고급 표 추출
    "pandas",         # 표 데이터 처리
]

[project.scripts]
convert = "convert_pdf_to_md:main"
```

---

## Troubleshooting

### 시스템 의존성 설치

```bash
# macOS
brew install opencv ghostscript

# Linux
apt-get install python3-opencv ghostscript
```

### 스캔본 PDF
```bash
ocrmypdf input.pdf output_ocr.pdf
mise run convert -- output_ocr.pdf
```

### 대용량 PDF
페이지별로 분할 후 개별 변환 또는 이미지 해상도 낮춤
