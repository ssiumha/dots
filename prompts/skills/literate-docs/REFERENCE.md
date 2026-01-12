# Literate Docs Reference

코드 내 문서화 skill의 참조 문서입니다.

## Literate Programming 배경

Donald Knuth (1984)가 제안한 프로그래밍 패러다임:

> "프로그램을 컴퓨터가 아닌 사람에게 설명하듯 작성하라"

이 skill은 완전한 Literate Programming이 아닌, 그 철학을 실용적으로 적용합니다:
- 코드 내 문서화 (docstring, 주석)
- 결정 이유 기록 (WHY, DECISION 태그)
- 언어별 표준 준수

## 리소스 구조

| 파일 | 내용 | 용도 |
|------|------|------|
| `01-codetags.md` | PEP 350 기반 코드 태그 | TODO, FIXME, WHY 등 |
| `02-python.md` | PEP 257 Docstrings | Python 문서화 |
| `03-javascript.md` | JSDoc/TSDoc | JS/TS 문서화 |
| `04-go.md` | GoDoc | Go 문서화 |
| `05-rust.md` | rustdoc | Rust 문서화 |
| `06-java.md` | Javadoc/KDoc | Java/Kotlin 문서화 |
| `07-markdown.md` | GitHub Alerts, Frontmatter | Markdown 문서화 |

## 권장 학습 순서

1. **공통**: `01-codetags.md` (모든 언어에 적용)
2. **주력 언어**: 해당 언어 리소스
3. **문서 파일**: `07-markdown.md` (README, ADR 등)

## 언어별 선택 가이드

| 상황 | 권장 리소스 |
|------|------------|
| Python 프로젝트 | 01 + 02 |
| Node.js/TS 프로젝트 | 01 + 03 |
| Go 프로젝트 | 01 + 04 |
| Rust 프로젝트 | 01 + 05 |
| Java/Spring 프로젝트 | 01 + 06 |
| 문서 중심 작업 | 01 + 07 |

## ADR (Architecture Decision Record) 연동

코드 내 DECISION 태그와 별도 ADR 문서를 연결:

```python
# DECISION: ORM 대신 raw SQL 사용
#   ADR: docs/adr/ADR-005-raw-sql.md
#   Context: 복잡한 집계 쿼리 성능
#   Choice: Raw SQL (0.1초 vs ORM 3초)
```

ADR 문서 구조는 `07-markdown.md`의 ADR 섹션 참조.

## 도구 생태계

### 문서 생성

| 언어 | 도구 | 명령 |
|------|------|------|
| Python | Sphinx | `sphinx-build -b html docs _build` |
| JS/TS | TypeDoc | `npx typedoc src/index.ts` |
| Go | godoc | `godoc -http=:6060` |
| Rust | cargo doc | `cargo doc --open` |
| Java | javadoc | `javadoc -d docs src/**/*.java` |

### 린트/검증

| 언어 | 도구 | 목적 |
|------|------|------|
| Python | pydocstyle | PEP 257 검증 |
| JS/TS | eslint-plugin-jsdoc | JSDoc 검증 |
| Go | staticcheck | godoc 검증 |
| Rust | clippy | missing_docs 린트 |
| Java | checkstyle | Javadoc 검증 |

### IDE 지원

| IDE | 확장/플러그인 |
|-----|--------------|
| VS Code | autoDocstring (Python), Document This (JS) |
| IntelliJ | 내장 Javadoc/KDoc 생성 |
| Vim | vim-doge (범용) |

## 트러블슈팅

| 문제 | 원인 | 해결 |
|------|------|------|
| 문서 생성 안 됨 | 주석 형식 오류 | 언어별 정확한 형식 확인 |
| 링크 깨짐 | 상대 경로 오류 | 절대 경로 또는 참조 확인 |
| 예시 실패 | doctest 오류 | 실제 실행 가능한 코드로 수정 |
| 태그 미인식 | IDE 설정 | TODO Highlight 확장 설치 |

## 컨벤션 통합

기존 프로젝트 컨벤션과 통합:

1. **기존 스타일 확인**: 프로젝트의 문서화 패턴 파악
2. **점진적 적용**: 새 코드부터 적용, 기존 코드는 수정 시
3. **팀 합의**: codetag 사용법 팀 내 공유
4. **자동화**: pre-commit hook으로 검증

## Best Practices 요약

1. **WHY 우선**: 왜 이렇게 했는지 설명
2. **티켓 연동**: TODO/FIXME에 이슈 번호
3. **만료일**: 임시 코드에 제거 예정일
4. **예시 필수**: 복잡한 API에 사용 예시
5. **링크 활용**: 관련 코드/문서 상호 참조
