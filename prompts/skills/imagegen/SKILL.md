---
name: imagegen
description: Generates and edits images via Gemini API. Use when creating new images or modifying existing ones.
---

# Image Generator

Gemini API를 활용한 이미지 생성 및 편집 스킬.

## Instructions

### 환경 설정

`GEMINI_API_KEY` 환경 변수 필요.

### 워크플로우 1: 이미지 생성

사용자가 "X 그려줘", "X 이미지 만들어줘" 요청 시:

1. **요청 분석**
   - 주제, 스타일, 분위기 파악
   - 모호하면 사용자에게 확인

2. **프롬프트 생성** (Claude가 직접 작성)
   - 항상 영어로 생성 (더 좋은 결과)
   - 구체적 디테일 추가 (조명, 분위기, 스타일)
   - 구조: `{주제} + {스타일} + {분위기} + {디테일}`

3. **모델 선택**
   - 기본: Gemini 2.0 Flash (무료)
   - 고품질 요청 시: Imagen 4 (유료)

4. **실행** (uv 사용)
   ```bash
   uv run scripts/generate.py --prompt "{프롬프트}" --output {파일명}.png
   ```

5. **결과 확인**
   - 이미지 경로 안내
   - 수정 필요시 프롬프트 조정 후 재생성

### 워크플로우 2: 이미지 편집

사용자가 "이 사진에서 X 바꿔줘", "배경 변경" 요청 시:

1. **입력 이미지 확인**
   - 이미지 경로 확인
   - 이미지 내용 파악 (Read로 확인)

2. **편집 프롬프트 생성**
   - 편집 의도를 명확한 영어 지시로 변환

3. **실행** (Gemini만 지원)
   ```bash
   uv run scripts/generate.py --prompt "{편집 지시}" --input {입력}.jpg --output {출력}.png
   ```

4. **결과 비교 및 반복**

## 프롬프트 생성 가이드

### 입력 → 프롬프트 변환 예시

| 사용자 요청 | 생성할 프롬프트 |
|-------------|-----------------|
| "고양이 그려줘" | "A cute fluffy cat sitting in a sunny spot, photorealistic, soft natural lighting" |
| "우주 고양이" | "A majestic cat floating in outer space, surrounded by colorful nebulae and stars, digital art style, dramatic lighting" |
| "수채화로 해변" | "A serene beach at sunset, watercolor painting style, soft pastel colors, artistic brush strokes, peaceful atmosphere" |
| "사이버펑크 도시" | "Cyberpunk cityscape at night, neon lights reflecting on wet streets, flying cars, futuristic architecture, cinematic lighting" |

### 편집 프롬프트 예시

| 편집 요청 | 생성할 프롬프트 |
|-----------|-----------------|
| "배경을 우주로" | "Change the background to outer space with stars and nebulae, keep the subject intact" |
| "선글라스 추가" | "Add stylish sunglasses to the person in the image" |
| "계절을 겨울로" | "Transform the scene to winter with snow covering the ground and trees" |

### 프롬프트 품질 원칙

1. **구체적으로**: "dog" 보다 "golden retriever puppy with fluffy fur"
2. **스타일 명시**: photorealistic, watercolor, oil painting, digital art, anime
3. **조명 설명**: soft lighting, dramatic shadows, golden hour, neon glow
4. **분위기 추가**: peaceful, energetic, mysterious, romantic
5. **구도 언급**: close-up portrait, wide landscape, bird's eye view

## CLI 사용법

PEP 723 inline script metadata로 의존성 자동 관리. uv로 바로 실행.

```bash
# 기본 (Gemini 2.0 Flash)
uv run scripts/generate.py -p "prompt" -o output.png

# Imagen 4 (고품질)
uv run scripts/generate.py -p "prompt" -o output.png -m imagen

# 이미지 편집
uv run scripts/generate.py -p "edit instruction" -i input.jpg -o output.png

# Imagen 옵션
--count N        # 여러 이미지 생성 (1-4)
--aspect 16:9    # 비율 (1:1, 16:9, 9:16, 4:3, 3:4)
```

## 모델 비교

| 모델 | ID | 장점 | 단점 |
|------|-----|------|------|
| Gemini 2.0 Flash (기본) | `gemini-2.0-flash-exp-image-generation` | 무료, 이미지 편집 | 할당량 공유 |
| Gemini 2.5 Flash | `gemini-2.5-flash-image` | 무료, 빠름 | 할당량 공유 |
| Gemini 3 Pro | `gemini-3-pro-image-preview` | 고품질 | 할당량 공유 |
| Imagen 4 | `imagen-4.0-generate-001` | 최고 품질 | 유료 전용 |

### 모델 변경 (환경 변수)

```bash
# Gemini 모델 변경
export IMAGEGEN_GEMINI_MODEL="gemini-2.5-flash-image"

# Imagen 모델 변경
export IMAGEGEN_IMAGEN_MODEL="imagen-4.0-fast-generate-001"
```

## 에러 처리

| 에러 | 원인 | 해결 |
|------|------|------|
| `RESOURCE_EXHAUSTED` | API 할당량 초과 | 표시된 시간만큼 대기 후 재시도 |
| `NOT_FOUND` | 모델 미지원 | 모델 ID 확인 |
| `GEMINI_API_KEY not set` | 환경 변수 없음 | `export GEMINI_API_KEY=...` |

## Examples

### 기본 이미지 생성
```
User: 고양이 그려줘
Assistant: 고양이 이미지를 생성하겠습니다.
[프롬프트: "A cute fluffy cat..."]
[uv run scripts/generate.py -p "..." -o cat.png]
이미지가 생성되었습니다: ./cat.png
```

### 스타일 지정
```
User: 수채화 스타일로 파리 에펠탑 그려줘
Assistant: 수채화 스타일의 에펠탑 이미지를 생성하겠습니다.
[프롬프트: "The Eiffel Tower in Paris, watercolor painting style..."]
```

### 이미지 편집
```
User: 이 사진에서 배경을 해변으로 바꿔줘
Assistant: 배경을 해변으로 변경하겠습니다.
[uv run scripts/generate.py -p "..." -i photo.jpg -o edited.png]
```
