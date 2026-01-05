# Available Tools

Sub-agent에서 사용할 수 있는 도구 목록입니다.

## 도구 카테고리

### File Operations

| 도구 | 용도 |
|------|------|
| `Read` | 파일 읽기 |
| `Write` | 파일 생성/덮어쓰기 |
| `Edit` | 파일 부분 수정 |
| `Glob` | 패턴으로 파일 검색 |
| `Grep` | 내용으로 파일 검색 |

### Execution

| 도구 | 용도 |
|------|------|
| `Bash` | 쉘 명령 실행 |
| `Task` | 다른 sub-agent 호출 (권장하지 않음) |

### User Interaction

| 도구 | 용도 |
|------|------|
| `AskUserQuestion` | 사용자에게 질문 |
| `TodoWrite` | 작업 목록 관리 |

### Web Access

| 도구 | 용도 |
|------|------|
| `WebFetch` | URL 내용 가져오기 |
| `WebSearch` | 웹 검색 |

### Notebook

| 도구 | 용도 |
|------|------|
| `NotebookEdit` | Jupyter 노트북 셀 편집 |

### MCP Integration

| 도구 | 용도 |
|------|------|
| `mcp__<server>__<tool>` | MCP 서버의 커스텀 도구 |

## 권장 도구 조합

### 읽기 전용 분석

```yaml
tools: Read, Grep, Glob
```

코드 리뷰, 보안 감사, 문서 분석 등 **파일을 수정하지 않는** 작업

### 코드 수정

```yaml
tools: Read, Edit, Write, Bash, Grep, Glob
```

버그 수정, 리팩토링, 테스트 실행 등 **파일 수정이 필요한** 작업

### 보안 감사

```yaml
tools: Read, Grep, Glob
permissionMode: plan
```

민감한 작업 전 **검토 단계**가 필요한 경우

### 전체 권한

```yaml
# tools 필드 생략
```

모든 도구를 main agent로부터 상속
