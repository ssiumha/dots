#!/usr/bin/env python3
"""
PDF to Markdown converter with table and image extraction support.
"""

import argparse
import sys
from pathlib import Path


def convert_pdf_to_markdown(pdf_path: Path, output_path: Path = None) -> None:
    """
    Convert PDF to Markdown format.

    Args:
        pdf_path: Path to input PDF file
        output_path: Path to output Markdown file (default: <pdf_name>.md)
    """
    if not pdf_path.exists():
        raise FileNotFoundError(f"PDF file not found: {pdf_path}")

    if output_path is None:
        output_path = pdf_path.with_suffix('.md')

    print(f"Converting {pdf_path} to {output_path}...")

    try:
        # Try PyMuPDF4LLM first (best quality)
        import pymupdf4llm

        md_text = pymupdf4llm.to_markdown(str(pdf_path))

        # Write output
        output_path.write_text(md_text, encoding='utf-8')

        print(f"✓ Successfully converted to {output_path}")
        print(f"  Backend: PyMuPDF4LLM")

    except ImportError:
        print("PyMuPDF4LLM not available, falling back to pdfplumber...")
        convert_with_pdfplumber(pdf_path, output_path)
    except Exception as e:
        print(f"PyMuPDF4LLM failed: {e}")
        print("Falling back to pdfplumber...")
        convert_with_pdfplumber(pdf_path, output_path)


def convert_with_pdfplumber(pdf_path: Path, output_path: Path) -> None:
    """
    Fallback conversion using pdfplumber.

    Args:
        pdf_path: Path to input PDF file
        output_path: Path to output Markdown file
    """
    import pdfplumber

    with pdfplumber.open(pdf_path) as pdf:
        markdown_lines = []

        for page_num, page in enumerate(pdf.pages, start=1):
            # Extract text
            text = page.extract_text()
            if text:
                markdown_lines.append(f"## Page {page_num}\n")
                markdown_lines.append(text)
                markdown_lines.append("\n")

            # Extract tables
            tables = page.extract_tables()
            if tables:
                for table in tables:
                    markdown_lines.append(convert_table_to_markdown(table))
                    markdown_lines.append("\n")

        # Write output
        output_path.write_text("\n".join(markdown_lines), encoding='utf-8')

        print(f"✓ Successfully converted to {output_path}")
        print(f"  Backend: pdfplumber")
        print(f"  Pages: {len(pdf.pages)}")


def convert_table_to_markdown(table: list) -> str:
    """
    Convert table data to Markdown table format.

    Args:
        table: List of lists representing table rows

    Returns:
        Markdown formatted table string
    """
    if not table:
        return ""

    lines = []

    # Header row
    header = table[0]
    lines.append("| " + " | ".join(str(cell or "") for cell in header) + " |")
    lines.append("| " + " | ".join("---" for _ in header) + " |")

    # Data rows
    for row in table[1:]:
        lines.append("| " + " | ".join(str(cell or "") for cell in row) + " |")

    return "\n".join(lines)


def main():
    """Main entry point for the CLI."""
    parser = argparse.ArgumentParser(
        description="Convert PDF to Markdown with table and image extraction"
    )
    parser.add_argument(
        "pdf_file",
        type=Path,
        help="Path to input PDF file"
    )
    parser.add_argument(
        "-o", "--output",
        type=Path,
        help="Path to output Markdown file (default: <pdf_name>.md)"
    )

    args = parser.parse_args()

    try:
        convert_pdf_to_markdown(args.pdf_file, args.output)
    except Exception as e:
        print(f"Error: {e}", file=sys.stderr)
        sys.exit(1)


if __name__ == "__main__":
    main()
