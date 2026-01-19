# 병렬 실행 (Default Parallel)

**기본 동작은 병렬**. 순차는 예외이며 정당화 필요.

> "If tasks don't depend on each other, why run them one by one?"
> — Anthropic: 3-5개 subagent 병렬 스핀업으로 **90% 시간 단축**

## 병렬 위임 (기본)

위임 시 **단일 메시지에 모든 Task 호출**:
```
# 탐색 3개 → 동시 호출
Task(Explore, "파일 A") + Task(Explore, "파일 B") + Task(Explore, "파일 C")

# 구현 + 테스트 + 리뷰 → 독립적이면 동시 호출
Task(general-purpose, "A.ts") + Task(general-purpose, "B.ts") + Task(Bash, "lint")
```

## 순차 위임 (예외, 정당화 필수)

다음 경우**만** 순차 실행:
1. **데이터 의존성**: A 결과 → B 입력
2. **파일 충돌**: 같은 파일 수정
3. **의미적 순서**: git add → commit → push

순차 실행 시 반드시 이유 명시:
```
# (순차: commit은 add 결과에 의존)
Task(Bash, "git add") → Task(Bash, "git commit")
```

## 배치 사이즈

| 조건 | 사이즈 | 근거 |
|------|--------|------|
| 최적 | 3-5개 | Anthropic 권장, merge 복잡도 최소 |
| 최대 | 10개 | 그 이상은 10개씩 분할 |

## 부분 실패 처리

N개 중 M개 실패 시:
1. 성공한 Task 결과 **유지**
2. 실패한 Task만 **단독 재위임** (원인 분석 추가)
3. 2회 연속 실패 → 순차 위임으로 전환

## 작업 분할 (Sectioning)

큰 작업은 **분할 후 병렬 위임**:

```
# 잘못된 예
Task(Explore, "전체 코드베이스 분석")  # 단일 에이전트에 과부하

# 올바른 예: 디렉토리별 분할
Task(Explore, "src/auth/") + Task(Explore, "src/api/") + Task(Explore, "src/utils/")

# 올바른 예: 파일 개수별 분할 (10개 파일 → 3-4개씩)
Task(general-purpose, "A,B,C.ts") + Task(general-purpose, "D,E,F.ts") + Task(general-purpose, "G,H,I,J.ts")
```

분할 기준:
- **디렉토리/모듈**: 독립적인 경계로 분할
- **파일 개수**: 3-5개 파일씩 묶음
- **기능 단위**: 관련 기능끼리 그룹
