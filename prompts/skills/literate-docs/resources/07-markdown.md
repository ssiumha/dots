# Markdown 문서화

Markdown 파일의 구조화된 문서화 패턴입니다.

## YAML Frontmatter

### 기본 형식

```yaml
---
title: 문서 제목
description: 문서 설명
author: 작성자
date: 2025-01-08
tags: [tag1, tag2]
---
```

### Skill/Prompt 파일

```yaml
---
name: skill-name
description: 언제 사용하는지 명시. 구체적 키워드 포함.
history:
  - 2025-01-08: 초기 생성
  - 2025-01-15: 기능 추가
decisions:
  - "선택 A: 이유 설명"
---
```

### 설정 문서

```yaml
---
version: "1.0"
last_updated: 2025-01-08
status: stable  # draft | stable | deprecated
breaking_changes:
  - version: "0.9"
    description: "API 변경"
---
```

## GitHub Alerts

GitHub에서 특별히 렌더링되는 알림 블록입니다.

### NOTE (참고)

```markdown
> [!NOTE]
> 이 설정은 0.30+ 버전에서만 지원됩니다.
```

### TIP (팁)

```markdown
> [!TIP]
> `fd` 사용 시 성능이 10배 향상됩니다.
```

### IMPORTANT (중요)

```markdown
> [!IMPORTANT]
> 프로덕션 배포 전 반드시 테스트하세요.
```

### WARNING (경고)

```markdown
> [!WARNING]
> 이 옵션은 다음 버전에서 deprecated 예정입니다.
```

### CAUTION (주의)

```markdown
> [!CAUTION]
> 이 작업은 되돌릴 수 없습니다. 데이터가 영구 삭제됩니다.
```

## 구조화 패턴

### README 템플릿

```markdown
# 프로젝트명

한 줄 설명.

## Quick Start

​```bash
npm install mylib
​```

## Features

- 기능 1
- 기능 2

## Installation

상세 설치 방법.

## Usage

사용 예시.

## Configuration

설정 옵션.

## Contributing

기여 가이드.

## License

MIT
```

### ADR (Architecture Decision Record)

```markdown
# ADR-001: 제목

## Status

Accepted | Proposed | Deprecated | Superseded by ADR-XXX

## Context

결정이 필요한 상황 설명.

## Decision

선택한 방안과 이유.

## Consequences

### Positive
- 장점 1
- 장점 2

### Negative
- 단점 1

### Risks
- 리스크 1

## Alternatives Considered

### Option A
- 설명
- 거부 이유

### Option B
- 설명
- 거부 이유
```

### CHANGELOG

```markdown
# Changelog

## [Unreleased]

### Added
- 새 기능

### Changed
- 변경사항

### Deprecated
- 폐기 예정

### Removed
- 제거됨

### Fixed
- 버그 수정

### Security
- 보안 패치

## [1.0.0] - 2025-01-08

### Added
- 초기 릴리즈
```

## Codetags in Markdown

### 인라인

```markdown
본문 중간에 TODO(#123): 작업 내용을 표시할 수 있습니다.

<!-- TODO: 이 섹션 보강 필요 -->
```

### 블록

```markdown
## Known Issues

- FIXME: 대용량 파일 처리 시 메모리 이슈
- TODO(#456): 캐싱 구현

## Technical Decisions

WHY: Docker 대신 Podman 사용
- 이유: rootless 보안
- 대안: Docker (널리 사용됨)
```

### HISTORY 블록

```markdown
## History

| 날짜 | 변경 | 담당 |
|------|------|------|
| 2025-01-08 | 초기 작성 | @alice |
| 2025-01-15 | 섹션 추가 | @bob |
```

## 코드 블록

### 언어 지정

```markdown
​```python
def hello():
    print("Hello, World!")
​```
```

### 파일명 표시

```markdown
​```python title="main.py"
def main():
    pass
​```
```

### 라인 하이라이트 (일부 렌더러)

```markdown
​```python {3-4}
def process():
    data = load()
    result = transform(data)  # 강조
    save(result)              # 강조
​```
```

### diff 표시

```markdown
​```diff
- old line
+ new line
​```
```

## 링크 패턴

### 상대 경로

```markdown
[설정 가이드](./docs/configuration.md)
[상위 문서](../README.md)
```

### 앵커 링크

```markdown
[설치 섹션으로](#installation)
[다른 문서의 섹션](./other.md#section-name)
```

### 참조 스타일

```markdown
자세한 내용은 [공식 문서][docs]를 참조하세요.

[docs]: https://example.com/docs
```

## 테이블

### 기본

```markdown
| 옵션 | 설명 | 기본값 |
|------|------|--------|
| `timeout` | 타임아웃 (초) | 30 |
| `retries` | 재시도 횟수 | 3 |
```

### 정렬

```markdown
| 왼쪽 | 가운데 | 오른쪽 |
|:-----|:------:|-------:|
| a    | b      | c      |
```

## 체크리스트

```markdown
## Tasks

- [x] 완료된 작업
- [ ] 미완료 작업
- [ ] TODO(#123): 티켓 연동 작업
```

## 접기 (Details)

```markdown
<details>
<summary>상세 내용 보기</summary>

숨겨진 내용입니다.

​```python
hidden_code()
​```

</details>
```

## Mermaid 다이어그램

```markdown
​```mermaid
graph LR
    A[시작] --> B{조건}
    B -->|Yes| C[처리]
    B -->|No| D[종료]
    C --> D
​```
```

## Best Practices

1. **Frontmatter 필수**: 메타데이터로 문서 관리
2. **GitHub Alerts 활용**: NOTE, WARNING 등 시각적 강조
3. **섹션 구조화**: 일관된 헤딩 레벨
4. **링크 검증**: 상대 경로 선호, 깨진 링크 방지
5. **코드 블록 언어 명시**: 구문 강조 활성화
