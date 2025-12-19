# [웹앱 프로젝트명]

[프로젝트 설명: 사용자 대상 웹 애플리케이션]

**스택**: Next.js 14 + TypeScript + Tailwind CSS

## 퀵 커맨드

```bash
# 개발 서버
npm run dev
# → http://localhost:3000

# 빌드 + 타입체크
npm run build

# 프로덕션 실행
npm start

# 테스트
npm test              # 유닛 테스트
npm run test:e2e      # E2E 테스트

# Lint & Format
npm run lint
npm run format
```

## 서비스

### 개발

- Frontend: http://localhost:3000
- API: http://localhost:3000/api
- Storybook: http://localhost:6006

### 프로덕션

- URL: https://example.com
- Admin: https://admin.example.com

## 환경변수

### 필수

```bash
# .env.local
NEXT_PUBLIC_API_URL=http://localhost:3000/api
DATABASE_URL=postgres://user:pass@localhost:5432/app_dev

# 인증
NEXTAUTH_SECRET=your-secret-here
NEXTAUTH_URL=http://localhost:3000

# 외부 서비스
STRIPE_SECRET_KEY=sk_test_...
```

### 선택

```bash
# .env.local (optional)
NEXT_PUBLIC_GA_ID=G-XXXXXXXXXX
SENTRY_DSN=https://...
```

## 인증 워크플로우

NextAuth.js 기반 인증:

1. 로그인: `/api/auth/signin`
2. 세션 확인: `useSession()` hook 사용
3. 보호된 라우트: `middleware.ts`에서 검증

```typescript
// 인증 필요 페이지
import { getServerSession } from 'next-auth'

export default async function ProtectedPage() {
  const session = await getServerSession()
  if (!session) redirect('/login')
  // ...
}
```

## 데이터베이스

### 마이그레이션

```bash
# 마이그레이션 생성
npx prisma migrate dev --name init

# 마이그레이션 적용 (프로덕션)
npx prisma migrate deploy

# Prisma Studio
npx prisma studio
```

### 시드

```bash
npx prisma db seed
```

## 배포

### Vercel

```bash
# 프리뷰 배포
git push origin feature-branch

# 프로덕션 배포
git push origin main
```

배포 전 체크:
- [ ] 환경변수 설정 완료
- [ ] 데이터베이스 마이그레이션 적용
- [ ] 빌드 성공 확인

## 프로젝트 특이사항

### 이미지 최적화

- Next.js Image 컴포넌트 사용 필수
- 외부 이미지는 `next.config.js`에 도메인 등록

### API Routes

- `/app/api/` 디렉토리 구조
- Route handlers는 App Router 패턴 사용

### 상태 관리

- Server State: React Query (TanStack Query)
- Client State: Zustand
- Form: React Hook Form + Zod

## 트러블슈팅

### 빌드 에러: "Module not found"

**원인**: 절대 경로 설정 문제

**해결**:
```bash
# tsconfig.json 확인
{
  "compilerOptions": {
    "paths": {
      "@/*": ["./src/*"]
    }
  }
}
```

### 개발 서버 느림

**원인**: Hot reload 과다

**해결**:
```bash
# .next 삭제 후 재시작
rm -rf .next
npm run dev
```

## 참고

- Next.js 문서: https://nextjs.org/docs
- Prisma 문서: https://www.prisma.io/docs
- NextAuth.js: https://next-auth.js.org/
