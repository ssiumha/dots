# CLI Usage

Living Docs CLI 스크립트 사용법입니다.

## 스크립트 구조

```
scripts/
├── ldoc                  # 메인 CLI 진입점
├── todo-list.sh          # TODO 목록 조회
├── todo-archive.sh       # 완료된 TODO 아카이브
├── health-check.sh       # 문서 건강도 체크
└── lib/
    ├── frontmatter.sh    # YAML 파싱 라이브러리
    └── utils.sh          # 공통 유틸리티
```

## 사용법

```bash
# 현황 요약
ldoc

# TODO 목록 조회
ldoc list
ldoc list -s pending
ldoc list --priority urgent

# 완료된 TODO 아카이브
ldoc archive              # dry-run (미리보기)
ldoc archive --execute    # 실제 실행

# 문서 건강도 체크
ldoc health
ldoc health --quick       # 빠른 체크
ldoc health --full        # 전체 체크
```

## Proactive Triggers

다음 키워드 감지 시 스크립트 실행을 제안합니다:

| 키워드 | 스크립트 | 설명 |
|--------|----------|------|
| "TODO 목록", "할 일", "뭐 해야" | `ldoc list` | TODO 목록 조회 |
| "정리", "아카이브", "completed 정리" | `ldoc archive` | 완료된 TODO 정리 |
| "건강도", "문서 상태", "중복 확인" | `ldoc health` | 문서 건강도 체크 |

## 의존성

- `yq`: YAML frontmatter 파싱
- `rg` (ripgrep): 빠른 파일 검색
- `jq`: JSON 처리 (선택)
