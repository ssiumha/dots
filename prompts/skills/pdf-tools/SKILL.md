---
name: pdf-tools
description: PDF 문서를 Markdown으로 변환합니다. PDF 파일을 읽고 분석하거나 내용을 요약해야 할 때 사용하세요. mise + uv 환경에서 실행됩니다.
---

# PDF Tools

PDF 문서를 Markdown 형식으로 변환하여 Claude가 읽고 처리할 수 있도록 합니다.

## Instructions

이 스킬을 다음 순서로 사용하세요:

### 1. 환경 준비 (최초 1회)
```bash
mise install
mise run setup
```

### 2. PDF 변환
```bash
mise run convert -- <pdf_path> -o <output_path>
```

**Parameters:**
- `<pdf_path>`: 변환할 PDF 파일 경로 (필수)
- `-o <output_path>`: 출력 Markdown 파일 경로 (선택, 기본값: `<pdf_name>.md`)

**Output:**
- Markdown 파일 (`.md`)
- 이미지 폴더 (`<basename>_images/`) - PDF에 이미지가 있을 경우
- 추출된 표는 Markdown 표 형식으로 포함

### 3. 변환 결과 확인
변환된 `.md` 파일을 Read 도구로 읽어서 사용자에게 제공하거나 추가 분석을 수행합니다.

## Examples

### Example 1: PDF를 Markdown으로 변환
```
User: "이 PDF를 Markdown으로 변환해줘"
Assistant:
1. mise run convert -- /path/to/document.pdf -o /path/to/document.md
2. Read /path/to/document.md
3. 사용자에게 변환 완료 알림 및 내용 요약 제공
```

### Example 2: PDF 내용 요약
```
User: "invoice.pdf 내용을 요약해줘"
Assistant:
1. mise run convert -- invoice.pdf
2. Read invoice.md
3. 내용을 분석하여 요약 제공
```

### Example 3: PDF에서 특정 정보 추출
```
User: "research_paper.pdf에서 실험 결과 부분만 보여줘"
Assistant:
1. mise run convert -- research_paper.pdf
2. Read research_paper.md
3. Markdown에서 실험 결과 섹션을 찾아 제공
```

## Implementation Status

**현재 구현된 파일:**
- `mise.toml` - mise 환경 설정

**구현 예정:**
- `convert_pdf_to_md.py` - PDF 변환 스크립트
- `pyproject.toml` - Python 의존성 정의
- `uv.lock` - 의존성 버전 고정

상세한 구현 사항과 기술 문서는 `REFERENCE.md`를 참조하세요.
