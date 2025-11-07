# [모노레포 프로젝트명]

[프로젝트 설명: 여러 앱과 패키지를 포함하는 모노레포]

**스택**: Turborepo + pnpm + TypeScript

## 구조

```
apps/
  web/          # Next.js 웹앱
  api/          # Express API 서버
  admin/        # 관리자 대시보드
packages/
  ui/           # 공유 UI 컴포넌트
  db/           # Prisma 스키마 + 클라이언트
  config/       # 공유 설정 (tsconfig, eslint 등)
  types/        # 공유 타입 정의
```

## 퀵 커맨드

```bash
# 전체 빌드
pnpm build

# 개발 서버 (전체)
pnpm dev

# 특정 앱만 실행
pnpm dev --filter=web
pnpm dev --filter=api

# 테스트
pnpm test              # 전체 테스트
pnpm test --filter=api # 특정 패키지만

# Lint (전체)
pnpm lint

# 타입체크 (전체)
pnpm typecheck

# 의존성 설치
pnpm install

# 워크스페이스에 패키지 추가
pnpm add <package> --filter=web
```

## 서비스

### 개발

- Web: http://localhost:3000
- API: http://localhost:3001
- Admin: http://localhost:3002

### 프로덕션

- Web: https://example.com
- API: https://api.example.com
- Admin: https://admin.example.com

## 환경변수

### 앱별 설정

각 앱은 자체 `.env` 파일 사용:

```bash
# apps/web/.env
NEXT_PUBLIC_API_URL=http://localhost:3001

# apps/api/.env
DATABASE_URL=postgres://user:pass@localhost:5432/app_dev
PORT=3001

# apps/admin/.env
NEXT_PUBLIC_API_URL=http://localhost:3001
```

### 공유 환경변수

루트 `.env` (선택):

```bash
# .env
NODE_ENV=development
```

## 워크스페이스 관리

### pnpm-workspace.yaml

```yaml
packages:
  - 'apps/*'
  - 'packages/*'
```

### 패키지 간 의존성

```json
// apps/web/package.json
{
  "dependencies": {
    "@repo/ui": "workspace:*",
    "@repo/db": "workspace:*"
  }
}
```

### 공유 설정

```json
// packages/config/tsconfig.json (base)
{
  "compilerOptions": {
    "strict": true,
    "esModuleInterop": true
  }
}

// apps/web/tsconfig.json (extends)
{
  "extends": "@repo/config/tsconfig.json",
  "compilerOptions": {
    "jsx": "preserve"
  }
}
```

## 빌드 최적화

### Turborepo 캐싱

```json
// turbo.json
{
  "pipeline": {
    "build": {
      "dependsOn": ["^build"],
      "outputs": ["dist/**", ".next/**"]
    },
    "dev": {
      "cache": false,
      "persistent": true
    },
    "test": {
      "dependsOn": ["build"],
      "outputs": ["coverage/**"]
    }
  }
}
```

### 빌드 순서

의존성 자동 해결:
1. `packages/*` 먼저 빌드
2. `apps/*` 빌드 (병렬)

## 데이터베이스

### Prisma 공유

`packages/db`에서 중앙 관리:

```bash
# 마이그레이션
cd packages/db
npx prisma migrate dev

# 클라이언트 생성
npx prisma generate
```

### 각 앱에서 사용

```typescript
import { prisma } from '@repo/db'

const users = await prisma.user.findMany()
```

## 배포

### Vercel (Web + Admin)

각 앱은 독립 배포:

```bash
# apps/web
vercel --prod

# apps/admin
vercel --prod
```

### API Server

PM2 또는 Docker로 배포:

```bash
# apps/api
pnpm build
pm2 start ecosystem.config.js
```

## 프로젝트 특이사항

### 공유 UI 컴포넌트

`packages/ui`에서 관리:

```typescript
// packages/ui/src/Button.tsx
export const Button = ({ children, ...props }) => {
  return <button {...props}>{children}</button>
}

// apps/web에서 사용
import { Button } from '@repo/ui'
```

### 타입 공유

`packages/types`에서 공통 타입 정의:

```typescript
// packages/types/src/user.ts
export interface User {
  id: string
  email: string
  name: string
}

// 모든 앱에서 import
import type { User } from '@repo/types'
```

### 코드 생성

Turborepo 태스크로 자동화:

```bash
# 새 앱 생성
pnpm turbo gen workspace --name=mobile

# 새 패키지 생성
pnpm turbo gen workspace --name=utils
```

## 트러블슈팅

### 빌드 캐시 문제

**증상**: 변경사항이 반영 안 됨

**해결**:
```bash
# 캐시 삭제
pnpm turbo run build --force

# 또는 전체 정리
rm -rf node_modules .turbo
pnpm install
```

### 타입 에러 (workspace 패키지)

**증상**: `Cannot find module '@repo/ui'`

**해결**:
```bash
# 전체 빌드
pnpm build

# 특정 패키지만 빌드
pnpm build --filter=@repo/ui
```

### pnpm 설치 실패

**증상**: `ERR_PNPM_PEER_DEP_ISSUES`

**해결**:
```bash
# strict 모드 해제
pnpm install --no-strict-peer-dependencies
```

## 참고

- Turborepo: https://turbo.build/repo/docs
- pnpm: https://pnpm.io/
- Monorepo 패턴: https://monorepo.tools/
