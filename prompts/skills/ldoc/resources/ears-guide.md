# EARS (Easy Approach to Requirements Syntax) 가이드

EARS는 요구사항을 명확하고 일관되게 작성하기 위한 구문 템플릿입니다.

## EARS 패턴 종류

### 1. Ubiquitous (보편적 요구사항)

**용도:** 항상 적용되는 기본 요구사항

**템플릿:**
```
The <system> shall <response>.
```

**예시:**
- "The authentication service shall encrypt all passwords using bcrypt."
- "시스템은 모든 API 응답에 요청 ID를 포함해야 한다."

**식별:** 조건 없이 항상 수행해야 하는 동작

---

### 2. State-Driven (상태 기반 요구사항)

**용도:** 특정 상태/조건이 유지되는 동안 적용

**템플릿:**
```
While <precondition>, the <system> shall <response>.
```

**예시:**
- "While the user is logged in, the system shall display the navigation menu."
- "사용자가 관리자 권한을 가진 동안, 시스템은 관리 메뉴를 표시해야 한다."

**식별 키워드:** "~하는 동안", "~인 상태에서", "while", "during"

---

### 3. Event-Driven (이벤트 기반 요구사항)

**용도:** 특정 이벤트/트리거가 발생했을 때 적용

**템플릿:**
```
When <trigger>, the <system> shall <response>.
```

**예시:**
- "When the user clicks the submit button, the system shall validate all form fields."
- "결제가 완료되면, 시스템은 확인 이메일을 발송해야 한다."

**식별 키워드:** "~하면", "~할 때", "when", "upon", "after"

---

### 4. Optional Feature (선택적 기능 요구사항)

**용도:** 특정 기능이 포함된 경우에만 적용

**템플릿:**
```
Where <feature>, the <system> shall <response>.
```

**예시:**
- "Where two-factor authentication is enabled, the system shall require a verification code."
- "다크 모드가 활성화된 경우, 시스템은 어두운 색상 테마를 적용해야 한다."

**식별 키워드:** "~가 있는 경우", "~가 활성화된 경우", "where", "if configured"

---

### 5. Unwanted Behavior (원치 않는 동작 방지)

**용도:** 원하지 않는 상황 발생 시 시스템이 취해야 할 조치

**템플릿:**
```
If <condition>, then the <system> shall <response>.
```

**예시:**
- "If the login attempt fails three times, then the system shall lock the account for 30 minutes."
- "네트워크 연결이 끊어지면, 시스템은 자동으로 재연결을 시도해야 한다."

**식별 키워드:** "~가 실패하면", "~가 발생하면", "if", "in case of"

---

### 6. Complex (복합 요구사항)

**용도:** 위 패턴들의 조합이 필요한 복잡한 요구사항

**템플릿:**
```
While <precondition>, when <trigger>, the <system> shall <response>.
Where <feature>, if <condition>, then the <system> shall <response>.
```

**예시:**
- "While the user is logged in, when the session expires, the system shall redirect to the login page."
- "알림 기능이 활성화된 경우, 새 메시지가 도착하면, 시스템은 푸시 알림을 전송해야 한다."

---

## 패턴 선택 가이드

| 상황 | 권장 패턴 |
|------|----------|
| 조건 없이 항상 수행 | Ubiquitous |
| 특정 상태 유지 중 | State-Driven |
| 특정 이벤트 발생 시 | Event-Driven |
| 선택적 기능 관련 | Optional Feature |
| 오류/예외 처리 | Unwanted Behavior |
| 위 조합 필요 | Complex |

---

## 품질 체크리스트

- [ ] **명확성**: 해석의 여지가 없는가?
- [ ] **검증 가능성**: 테스트로 확인할 수 있는가?
- [ ] **완전성**: 필요한 모든 정보가 포함되었는가?
- [ ] **일관성**: 다른 요구사항과 충돌하지 않는가?
- [ ] **추적 가능성**: 관련 문서와 연결되어 있는가?

---

## 피해야 할 패턴

**모호한 표현:**
- "빠르게", "적절하게", "가능한 한"

**여러 요구사항 혼합:**
- 하나의 문장에 여러 요구사항

**검증 불가능:**
- "사용자 친화적이어야 한다"

**구현 종속:**
- 특정 기술/방법 강제

### 좋은 예시 vs 나쁜 예시

| 나쁜 예시 | 좋은 예시 |
|----------|----------|
| 시스템은 빠르게 응답해야 한다 | 시스템은 모든 API 요청에 3초 이내 응답해야 한다 |
| 사용자가 쉽게 사용할 수 있어야 한다 | When the user submits a search query, the system shall display results within 2 seconds |

---

## 요구사항 분류

| 분류 | 설명 | 예시 |
|------|------|------|
| **Functional** | 시스템이 무엇을 해야 하는지 | 사용자 인터페이스 동작, 데이터 처리 규칙 |
| **Non-Functional** | 시스템이 어떻게 동작해야 하는지 | 성능, 보안, 가용성, 확장성 |
| **Constraint** | 시스템 개발/운영에 대한 제한 | 기술 스택 제한, 규정 준수 |
| **Interface** | 외부 시스템과의 상호작용 | API 스펙, 데이터 형식, 프로토콜 |

---

## 상태 관리

| 상태 | 설명 | 다음 상태 |
|------|------|----------|
| draft | 초안 작성됨 | proposed |
| proposed | 검토 요청됨 | approved, draft |
| approved | 승인됨, 구현 예정 | implemented, deprecated |
| implemented | 구현 완료 | deprecated |
| deprecated | 더 이상 유효하지 않음 | - |

---

## Living Docs 연동

### 요구사항 → TODO 변환

승인된 요구사항을 구현 TODO로 변환:

```markdown
# TODO: implement-req-{slug}

## 작업 내용
[[req-{slug}]] 요구사항 구현

## 완료 조건
- [ ] 요구사항 수용 기준 모두 충족
- [ ] 관련 테스트 통과
- [ ] req-{slug} 상태를 implemented로 변경
```

### 요구사항 ↔ 의사결정 연결

- 요구사항에서: `references: [dec-xxx]`
- 의사결정에서: `impacts: [req-xxx]`
