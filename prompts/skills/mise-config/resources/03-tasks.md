# Tasks Section

mise tasks provide a built-in task runner similar to `make`, `just`, or npm scripts.

## Basic Syntax (TOML)

```toml
[tasks.build]
description = "Build the project"
run = "npm run build"

[tasks.test]
description = "Run tests"
run = "npm test"
```

Run with: `mise run build` or `mise run test`

## Task Options

### run

The command(s) to execute:

```toml
# Single command
[tasks.build]
run = "npm run build"

# Multiple commands (sequential)
[tasks.build]
run = ["npm run lint", "npm run build", "npm test"]

# Multi-line command
[tasks.deploy]
run = """
#!/usr/bin/env bash
set -e
npm run build
docker build -t myapp .
docker push myapp
"""
```

### description

Help text shown in `mise tasks`:

```toml
[tasks.build]
description = "Build the project for production"
run = "npm run build"
```

### depends

Run other tasks first:

```toml
[tasks.build]
depends = ["lint", "test"]
run = "npm run build"

[tasks.lint]
run = "npm run lint"

[tasks.test]
run = "npm test"
```

Dependencies run in the order listed. Tasks in the same array position run sequentially:

```toml
[tasks.deploy]
depends = ["build", "test", "package"]  # build → test → package (sequential)
run = "echo Done"

[tasks.ci]
depends = ["lint", "test"]              # lint → test (sequential)
run = "echo CI passed"
```

For parallel execution, use multiple task definitions or background processes in the task script.

### alias

Shorthand names:

```toml
[tasks.build]
alias = "b"
run = "npm run build"
```

Run with: `mise run b`

### env

Task-specific environment variables:

```toml
[tasks.test]
env = { NODE_ENV = "test", CI = "true" }
run = "npm test"
```

### dir

Run in a specific directory:

```toml
[tasks.build-frontend]
dir = "frontend"
run = "npm run build"

[tasks.build-backend]
dir = "backend"
run = "go build"
```

### sources / outputs (Incremental Builds)

Skip task if outputs are newer than sources:

```toml
[tasks.build]
sources = ["src/**/*.ts", "package.json", "tsconfig.json"]
outputs = ["dist/**/*.js"]
run = "npm run build"
```

mise will:
- Check modification times
- Skip build if all outputs are newer than all sources
- Run if any source changed or output missing

### usage (Arguments)

Define task arguments:

```toml
[tasks.deploy]
usage = "mise run deploy <environment> [region]"
run = """
#!/usr/bin/env bash
ENV=$1
REGION=${2:-us-east-1}
echo "Deploying to $ENV in $REGION"
kubectl apply -f manifests/$ENV
"""
```

Call with: `mise run deploy production us-west-2`

### hide

Hide from `mise tasks` list:

```toml
[tasks.internal-cleanup]
hide = true
run = "rm -rf /tmp/build-cache"
```

## File-Based Tasks

For complex scripts, use separate executable files in `.mise/tasks/` or `mise/tasks/`.

### Basic File Task

`.mise/tasks/deploy`:

```bash
#!/usr/bin/env bash
#MISE description="Deploy to production"
#MISE alias="d"
#MISE depends=["build", "test"]

set -e

echo "Deploying..."
docker build -t myapp .
docker push myapp:latest
kubectl rollout restart deployment/myapp
```

Make it executable:
```bash
chmod +x .mise/tasks/deploy
```

Run with: `mise run deploy` or `mise run d`

### File Task with Subdirectories

Organize tasks in subdirectories:

```
.mise/tasks/
├── build
├── test
├── docker/
│   ├── build
│   ├── push
│   └── run
└── k8s/
    ├── apply
    └── rollout
```

Run with: `mise run docker:build`, `mise run k8s:apply`

### Ruby-Based Tasks

`.mise/tasks/analyze`:

```ruby
#!/usr/bin/env ruby
#MISE description="Analyze codebase"
#MISE depends=["test"]

require 'json'

# Your logic here
stats = {
  files: Dir.glob('**/*.rb').size,
  lines: `wc -l **/*.rb`.split.first.to_i
}

puts JSON.pretty_generate(stats)
```

### Python-Based Tasks

`.mise/tasks/check`:

```python
#!/usr/bin/env python3
#MISE description="Run all checks"
#MISE depends=["lint", "test"]

import subprocess
import sys

checks = [
    ("Type check", ["mypy", "src"]),
    ("Format check", ["black", "--check", "src"]),
    ("Security scan", ["bandit", "-r", "src"])
]

for name, cmd in checks:
    print(f"Running {name}...")
    result = subprocess.run(cmd)
    if result.returncode != 0:
        print(f"❌ {name} failed")
        sys.exit(1)

print("✅ All checks passed")
```

## Common Patterns

### Build Pipeline

```toml
[tasks.ci]
description = "Full CI pipeline"
depends = [["lint"], ["test"], ["build"]]

[tasks.lint]
run = "npm run lint"

[tasks.test]
run = "npm test"
sources = ["src/**/*.ts", "test/**/*.ts"]

[tasks.build]
run = "npm run build"
sources = ["src/**/*.ts", "package.json"]
outputs = ["dist/**/*.js"]
```

### Development Workflow

```toml
[tasks.dev]
description = "Start development server"
run = "npm run dev"

[tasks.dev:full]
description = "Start all services (frontend + backend + db)"
depends = ["db:start"]
run = ["npm run dev:backend &", "npm run dev:frontend"]

[tasks.db:start]
run = "docker-compose up -d postgres redis"
```

### Deployment

```toml
[tasks.deploy]
description = "Deploy to production"
depends = [["build"], ["test"], ["push"]]
run = "kubectl apply -f k8s/"

[tasks.deploy:staging]
description = "Deploy to staging"
env = { DEPLOY_ENV = "staging" }
depends = ["build"]
run = "kubectl apply -f k8s/ -n staging"
```

### Database Tasks

```toml
[tasks.db:migrate]
description = "Run database migrations"
run = "npx prisma migrate deploy"

[tasks.db:seed]
description = "Seed database"
depends = ["db:migrate"]
run = "npx prisma db seed"

[tasks.db:reset]
description = "Reset database (DESTRUCTIVE)"
run = "npx prisma migrate reset --force"
```

### Code Quality

```toml
[tasks.lint]
description = "Lint code"
run = ["eslint src", "prettier --check src"]
sources = ["src/**/*.ts", ".eslintrc.js"]

[tasks.lint:fix]
description = "Fix linting issues"
run = ["eslint src --fix", "prettier --write src"]

[tasks.format]
alias = "fmt"
run = "prettier --write ."
```

### Testing

```toml
[tasks.test]
description = "Run all tests"
run = "npm test"
sources = ["src/**/*.ts", "test/**/*.ts"]

[tasks.test:unit]
run = "npm test -- --testPathPattern=unit"

[tasks.test:integration]
depends = ["db:start"]
run = "npm test -- --testPathPattern=integration"

[tasks.test:e2e]
depends = ["build"]
run = "playwright test"

[tasks.test:watch]
run = "npm test -- --watch"
```

### Docker Tasks

```toml
[tasks.docker:build]
description = "Build Docker image"
run = "docker build -t myapp:latest ."
sources = ["Dockerfile", "src/**/*", "package.json"]

[tasks.docker:push]
depends = ["docker:build"]
run = "docker push myapp:latest"

[tasks.docker:run]
run = "docker run -p 3000:3000 myapp:latest"
```

## Watch Mode

Watch for changes and re-run tasks:

```bash
mise watch -t build
mise watch -t test
```

This uses `watchexec` (must be installed).

### Watch-Specific Task

```toml
[tasks.watch:tests]
description = "Watch and run tests"
run = "watchexec -e ts,tsx -- npm test"
```

## Task Outputs

### Capturing Output

```toml
[tasks.version]
run = "git describe --tags"
# Output goes to stdout
```

Use in scripts:
```bash
VERSION=$(mise run version)
```

### Quiet Mode

```bash
mise run -q build  # Suppress task output
```

## Examples

### Full-Stack JavaScript

```toml
[tasks.install]
alias = "i"
run = "pnpm install"

[tasks.dev]
description = "Start development servers"
run = ["pnpm --filter backend dev", "pnpm --filter frontend dev"]

[tasks.build]
depends = ["build:backend", "build:frontend"]

[tasks.build:backend]
dir = "backend"
run = "pnpm build"
sources = ["src/**/*.ts"]
outputs = ["dist/**/*.js"]

[tasks.build:frontend]
dir = "frontend"
run = "pnpm build"
sources = ["src/**/*.tsx", "public/**/*"]
outputs = ["dist/**/*"]

[tasks.test]
depends = [["test:backend"], ["test:frontend"]]

[tasks.test:backend]
dir = "backend"
run = "pnpm test"

[tasks.test:frontend]
dir = "frontend"
run = "pnpm test"
```

### Python Data Pipeline

```toml
[tasks.install]
run = "pip install -r requirements.txt"

[tasks.extract]
description = "Extract data from sources"
run = "python scripts/extract.py"
outputs = ["data/raw/*.csv"]

[tasks.transform]
description = "Transform raw data"
depends = ["extract"]
run = "python scripts/transform.py"
sources = ["data/raw/*.csv"]
outputs = ["data/processed/*.parquet"]

[tasks.load]
description = "Load data to warehouse"
depends = ["transform"]
run = "python scripts/load.py"

[tasks.pipeline]
description = "Run full ETL pipeline"
depends = [["extract"], ["transform"], ["load"]]
```

## Best Practices

1. **Use depends for order**: Let mise handle sequencing
2. **Leverage sources/outputs**: Avoid unnecessary rebuilds
3. **Add descriptions**: Help teammates understand tasks
4. **Use file tasks for complex logic**: Keep TOML tasks simple
5. **Organize with prefixes**: `test:unit`, `test:integration`, `docker:build`
6. **Add aliases for common tasks**: `b` for build, `t` for test
7. **Use env for task-specific config**: Avoid hardcoding values

## Troubleshooting

### Task not found

```bash
mise tasks  # List all tasks
```

Check:
- Is the file executable? `chmod +x .mise/tasks/mytask`
- Is the file in the right directory? `.mise/tasks/` or `mise/tasks/`

### Dependencies not running

```toml
# ❌ Wrong - depends expects array of strings
[tasks.build]
depends = "lint"

# ✅ Correct
[tasks.build]
depends = ["lint"]
```

### Sources/outputs not working

- Glob patterns must match at least one file when task is defined
- Check paths are relative to config root
- Use `mise run --verbose` to debug

### Task runs every time (ignoring sources/outputs)

- Ensure all source patterns match existing files
- Check output paths are correct
- Verify outputs are actually created by the task
