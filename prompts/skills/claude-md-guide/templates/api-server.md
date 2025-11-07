# [API 서버 프로젝트명]

[프로젝트 설명: RESTful API 서버 또는 GraphQL 서버]

**스택**: Express + TypeScript + Prisma + PostgreSQL

## 퀵 커맨드

```bash
# 개발 서버
npm run dev
# → http://localhost:3000

# 빌드
npm run build

# 프로덕션 실행
npm start

# 테스트
npm test              # 유닛 테스트
npm run test:e2e      # E2E 테스트 (DB 포함)

# Lint & 타입체크
npm run lint
npm run typecheck

# CI 전체
just ci               # lint + typecheck + test
```

## 서비스

### 개발

- API: http://localhost:3000
- Health: http://localhost:3000/health
- Docs: http://localhost:3000/api-docs (Swagger)
- PostgreSQL: localhost:5432
- Redis: localhost:6379

### 프로덕션

- API: https://api.example.com
- Health: https://api.example.com/health

## 환경변수

### 필수

```bash
# .env
NODE_ENV=development
PORT=3000

# 데이터베이스
DATABASE_URL=postgres://user:pass@localhost:5432/app_dev

# Redis
REDIS_URL=redis://localhost:6379

# 인증
JWT_SECRET=your-secret-here
JWT_EXPIRES_IN=7d

# 외부 API
STRIPE_SECRET_KEY=sk_test_...
SENDGRID_API_KEY=SG...
```

### 선택

```bash
# .env (optional)
LOG_LEVEL=debug
RATE_LIMIT_MAX=100
CORS_ORIGIN=http://localhost:3001
```

## 데이터베이스

### 마이그레이션

```bash
# 개발: 마이그레이션 생성 + 적용
npx prisma migrate dev --name add_users

# 프로덕션: 마이그레이션 적용
npx prisma migrate deploy

# 마이그레이션 리셋 (개발 전용)
npx prisma migrate reset
```

### 백업/복구

```bash
# 백업
pg_dump $DATABASE_URL > backup.sql

# 복구
psql $DATABASE_URL < backup.sql
```

## 인증 워크플로우

JWT 기반 인증:

1. **회원가입**: `POST /api/auth/signup`
2. **로그인**: `POST /api/auth/login` → JWT 토큰 발급
3. **인증**: `Authorization: Bearer <token>` 헤더 필수
4. **갱신**: `POST /api/auth/refresh`

```typescript
// 보호된 엔드포인트
app.get('/api/users/me', authenticate, async (req, res) => {
  const user = req.user // authenticate 미들웨어에서 주입
  res.json(user)
})
```

## API 문서

### Swagger UI

개발 환경에서 자동 생성:
- URL: http://localhost:3000/api-docs

### 주요 엔드포인트

```
POST   /api/auth/signup      # 회원가입
POST   /api/auth/login       # 로그인
GET    /api/users            # 사용자 목록
GET    /api/users/:id        # 사용자 조회
PUT    /api/users/:id        # 사용자 수정
DELETE /api/users/:id        # 사용자 삭제
```

## 배포

### PM2

```bash
# 시작
pm2 start ecosystem.config.js

# 재시작 (Zero-downtime)
pm2 reload ecosystem.config.js

# 로그
pm2 logs api

# 모니터링
pm2 monit
```

### 배포 스크립트

```bash
# 배포
just deploy

# 롤백
just rollback
```

## 프로젝트 특이사항

### Rate Limiting

- IP당 100 req/min (기본)
- 인증 엔드포인트: 5 req/min
- Redis로 상태 공유

### 에러 처리

모든 에러는 중앙 핸들러에서 처리:

```json
{
  "error": {
    "code": "VALIDATION_ERROR",
    "message": "Invalid email format",
    "details": [...]
  }
}
```

### 로깅

- 개발: Console (색상 포함)
- 프로덕션: JSON 포맷 (Sentry 연동)

## 테스트

### E2E 테스트

실제 PostgreSQL 컨테이너 사용:

```bash
# compose.test.yaml로 DB 시작
just test-e2e
```

각 테스트 전 DB 초기화됨.

### 테스트 DB

```bash
# 테스트 DB 시작
docker compose -f compose.test.yaml up -d

# 테스트 실행
DATABASE_URL=postgres://test_user:test_pass@localhost:5433/test_db npm run test:e2e

# 정리
docker compose -f compose.test.yaml down -v
```

## 트러블슈팅

### DB 연결 실패

**증상**: `Error: Can't reach database server`

**해결**:
```bash
# DB 상태 확인
docker compose ps

# 재시작
docker compose restart db
```

### 마이그레이션 충돌

**증상**: `Migration failed to apply`

**해결**:
```bash
# 개발 환경에서만
npx prisma migrate reset
npx prisma migrate dev
```

## 참고

- Express: https://expressjs.com/
- Prisma: https://www.prisma.io/docs
- JWT: https://jwt.io/
