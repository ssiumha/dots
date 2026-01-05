#!/usr/bin/env python3
# /// script
# requires-python = ">=3.11"
# dependencies = [
#     "google-genai>=1.0.0",
#     "Pillow>=10.0.0",
# ]
# ///
"""Gemini API 이미지 생성 CLI

Usage:
    # uv로 실행 (권장)
    uv run generate.py --prompt "A cat on a beach" --output cat.png

    # Image Editing
    uv run generate.py --prompt "Add sunglasses" --input photo.jpg --output edited.png

    # Imagen 4 (고품질, 유료)
    uv run generate.py --prompt "..." --output out.png --model imagen

Environment:
    GEMINI_API_KEY: Gemini API 키 (필수)
"""
import argparse
import os
import re
import sys
from io import BytesIO
from pathlib import Path

from google import genai
from google.genai import types
from PIL import Image

# 모델 이름 (환경 변수로 오버라이드 가능)
GEMINI_MODEL = os.environ.get("IMAGEGEN_GEMINI_MODEL", "gemini-2.0-flash-exp-image-generation")
IMAGEN_MODEL = os.environ.get("IMAGEGEN_IMAGEN_MODEL", "imagen-4.0-generate-001")


def generate_with_gemini(prompt: str, input_image: Path | None = None) -> bytes:
    """Gemini로 이미지 생성/편집"""
    client = genai.Client(api_key=os.environ["GEMINI_API_KEY"])

    contents = []
    if input_image:
        img = Image.open(input_image)
        contents.append(img)
    contents.append(prompt)

    response = client.models.generate_content(
        model=GEMINI_MODEL,
        contents=contents,
        config=types.GenerateContentConfig(response_modalities=["TEXT", "IMAGE"]),
    )

    if not response.candidates:
        raise ValueError("Empty response from API")

    for part in response.candidates[0].content.parts:
        if part.inline_data:
            return part.inline_data.data

    raise ValueError("No image in response")


def generate_with_imagen(
    prompt: str, count: int = 1, aspect: str = "1:1"
) -> list[bytes]:
    """Imagen 4로 고품질 이미지 생성"""
    client = genai.Client(api_key=os.environ["GEMINI_API_KEY"])

    response = client.models.generate_images(
        model=IMAGEN_MODEL,
        prompt=prompt,
        config=types.GenerateImagesConfig(
            number_of_images=count,
            aspect_ratio=aspect,
        ),
    )

    return [img.image.image_bytes for img in response.generated_images]


def main() -> int:
    parser = argparse.ArgumentParser(
        description="Generate images with Gemini API",
        formatter_class=argparse.RawDescriptionHelpFormatter,
        epilog=f"""
Models:
  gemini  {GEMINI_MODEL} (기본, 무료, 편집 지원)
  imagen  {IMAGEN_MODEL} (고품질, 유료)

Examples:
  uv run %(prog)s -p "A sunset over mountains" -o sunset.png
  uv run %(prog)s -p "Add a rainbow" -i photo.jpg -o edited.png
  uv run %(prog)s -p "Portrait" -o portrait.png -m imagen --aspect 3:4
        """,
    )
    parser.add_argument(
        "--prompt", "-p", required=True, help="Image description or edit instruction"
    )
    parser.add_argument(
        "--output", "-o", required=True, help="Output file path (PNG recommended)"
    )
    parser.add_argument(
        "--input", "-i", help="Input image for editing (Gemini model only)"
    )
    parser.add_argument(
        "--model",
        "-m",
        default="gemini",
        choices=["gemini", "imagen"],
        help="Model to use (default: gemini)",
    )
    parser.add_argument(
        "--count", "-n", type=int, default=1, help="Number of images (Imagen only)"
    )
    parser.add_argument(
        "--aspect",
        default="1:1",
        choices=["1:1", "16:9", "9:16", "4:3", "3:4"],
        help="Aspect ratio (Imagen only)",
    )

    args = parser.parse_args()

    if "GEMINI_API_KEY" not in os.environ:
        print("Error: GEMINI_API_KEY environment variable not set", file=sys.stderr)
        print("  export GEMINI_API_KEY='your-api-key'", file=sys.stderr)
        return 1

    if args.input and args.model == "imagen":
        parser.error("Image editing only supported with Gemini model")

    if args.input and not Path(args.input).exists():
        print(f"Error: 입력 파일을 찾을 수 없습니다: {args.input}", file=sys.stderr)
        return 1

    output = Path(args.output)
    output.parent.mkdir(parents=True, exist_ok=True)

    try:
        if args.model == "gemini":
            input_path = Path(args.input) if args.input else None
            image_bytes = generate_with_gemini(args.prompt, input_path)

            img = Image.open(BytesIO(image_bytes))
            img.save(output)
            print(f"Saved: {output}")
        else:
            images = generate_with_imagen(args.prompt, args.count, args.aspect)

            for i, img_bytes in enumerate(images):
                if len(images) > 1:
                    path = output.with_stem(f"{output.stem}_{i + 1}")
                else:
                    path = output

                img = Image.open(BytesIO(img_bytes))
                img.save(path)
                print(f"Saved: {path}")

    except Exception as e:
        error_msg = str(e)
        if "RESOURCE_EXHAUSTED" in error_msg or "429" in error_msg:
            print("Error: API 할당량 초과. 잠시 후 다시 시도하세요.", file=sys.stderr)
            if "retry" in error_msg.lower():
                match = re.search(r"retry.*?(\d+\.?\d*)\s*s", error_msg.lower())
                if match:
                    print(f"  권장 대기 시간: {match.group(1)}초", file=sys.stderr)
        elif "NOT_FOUND" in error_msg or "404" in error_msg:
            print("Error: 모델을 찾을 수 없습니다.", file=sys.stderr)
            print(f"  현재 설정: {GEMINI_MODEL if args.model == 'gemini' else IMAGEN_MODEL}", file=sys.stderr)
        elif "INVALID_ARGUMENT" in error_msg or "billed" in error_msg.lower():
            print("Error: Imagen은 유료 API 키가 필요합니다.", file=sys.stderr)
            print("  https://aistudio.google.com/ 에서 billing 설정", file=sys.stderr)
        else:
            print(f"Error: {e}", file=sys.stderr)
        return 1

    return 0


if __name__ == "__main__":
    sys.exit(main())
