# Environment Variables Section

The `[env]` section defines environment variables for your project.

## Basic Syntax

```toml
[env]
NODE_ENV = "production"
API_URL = "https://api.example.com"
LOG_LEVEL = "info"
```

## Template Variables

mise provides template variables for dynamic values:

| Variable | Description | Example Value |
|----------|-------------|---------------|
| `{{config_root}}` | Directory containing mise.toml | `/home/user/myproject` |
| `{{cwd}}` | Current working directory | `/home/user/myproject/subdir` |
| `{{env.VAR}}` | Existing environment variable | `{{env.HOME}}` → `/home/user` |
| `{{exec(cmd)}}` | Command output | `{{exec(git rev-parse HEAD)}}` |

### Examples

```toml
[env]
PROJECT_ROOT = "{{config_root}}"
HOME_DIR = "{{env.HOME}}"
GIT_SHA = "{{exec(git rev-parse --short HEAD)}}"
```

## Python Virtual Environment

Special syntax for Python projects:

```toml
[env]
_.python.venv = { path = ".venv", create = true }
```

This:
- Creates a virtualenv at `.venv` if it doesn't exist
- Activates it automatically when entering the directory
- Works with `python_venv_auto_create` setting

Alternative forms:

```toml
# Automatic path based on Python version
[env]
_.python.venv = "auto"

# Custom path
[env]
_.python.venv = ".venv"

# With auto-creation
[env]
_.python.venv = { path = ".venv", create = true }
```

## Common Patterns

### Development Environment

```toml
[env]
NODE_ENV = "development"
DEBUG = "true"
LOG_LEVEL = "debug"
PORT = "3000"
```

### Production Environment

```toml
[env]
NODE_ENV = "production"
LOG_LEVEL = "info"
SENTRY_DSN = "{{env.SENTRY_DSN}}"  # From system env
```

### Database Configuration

```toml
[env]
DATABASE_URL = "postgresql://localhost/myapp_dev"
REDIS_URL = "redis://localhost:6379/0"
```

### Build Configuration

```toml
[env]
BUILD_TARGET = "production"
SOURCEMAP = "true"
OUTPUT_DIR = "{{config_root}}/dist"
```

### API Keys (Use System Environment)

```toml
[env]
# ❌ DON'T hardcode secrets
API_KEY = "abc123secret"

# ✅ DO reference system environment
API_KEY = "{{env.API_KEY}}"
OPENAI_API_KEY = "{{env.OPENAI_API_KEY}}"
```

## Environment-Specific Overrides

Use separate files for different environments:

### mise.toml (defaults)

```toml
[env]
NODE_ENV = "development"
API_URL = "http://localhost:3000"
```

### mise.production.toml

```toml
[env]
NODE_ENV = "production"
API_URL = "https://api.example.com"
```

Activate with:
```bash
mise use --env production
```

## Path Manipulation

### Adding to PATH

```toml
[env]
PATH = ["{{config_root}}/bin", "{{env.PATH}}"]
```

This prepends `./bin` to PATH.

### Multiple Paths

```toml
[env]
PATH = [
  "{{config_root}}/bin",
  "{{config_root}}/scripts",
  "{{env.PATH}}"
]
```

## Tool-Specific Environment

### Node.js

```toml
[env]
NODE_ENV = "production"
NODE_OPTIONS = "--max-old-space-size=4096"
npm_config_registry = "https://registry.npmjs.org/"
```

### Python

```toml
[env]
PYTHONPATH = "{{config_root}}/src"
PYTHONUNBUFFERED = "1"
PIP_INDEX_URL = "https://pypi.org/simple"
_.python.venv = { path = ".venv", create = true }
```

### Go

```toml
[env]
GOPATH = "{{config_root}}/.go"
GOCACHE = "{{config_root}}/.cache/go-build"
CGO_ENABLED = "0"
```

### Rust

```toml
[env]
CARGO_TARGET_DIR = "{{config_root}}/target"
RUSTFLAGS = "-C target-cpu=native"
```

## Examples

### Full-Stack Application

```toml
[env]
# Common
NODE_ENV = "development"
LOG_LEVEL = "debug"
PROJECT_ROOT = "{{config_root}}"

# Frontend
VITE_API_URL = "http://localhost:3001"
VITE_WS_URL = "ws://localhost:3001"

# Backend
PORT = "3001"
DATABASE_URL = "postgresql://localhost/myapp_dev"
JWT_SECRET = "{{env.JWT_SECRET}}"

# External Services
REDIS_URL = "redis://localhost:6379/0"
OPENAI_API_KEY = "{{env.OPENAI_API_KEY}}"
```

### Monorepo

```toml
[env]
# Shared
WORKSPACE_ROOT = "{{config_root}}"
NODE_ENV = "development"

# Package-specific (use in subdirectories)
PACKAGE_NAME = "{{exec(basename $PWD)}}"
BUILD_OUTPUT = "{{config_root}}/dist/{{env.PACKAGE_NAME}}"
```

### CI/CD Integration

```toml
[env]
# Local development
CI = "false"
DEPLOYMENT_ENV = "local"

# Overridden in CI
# CI = "{{env.CI}}"                    # GitHub Actions sets this
# DEPLOYMENT_ENV = "{{env.GITHUB_REF}}"  # Branch/tag name
```

## Best Practices

1. **Never commit secrets**: Use `{{env.SECRET}}` to reference system environment
2. **Use templates for paths**: `{{config_root}}` makes configs portable
3. **Document expected variables**: Add comments for variables that must be set
4. **Separate by environment**: Use `mise.<env>.toml` for env-specific values
5. **Use mise.local.toml for overrides**: Gitignore this file for local customization

## Troubleshooting

### Variable not set

```bash
# Check what mise sees
mise env

# Check specific variable
mise env | grep MY_VAR
```

### Template not expanding

```toml
# ❌ Wrong syntax
VAR = "${config_root}/bin"

# ✅ Correct syntax
VAR = "{{config_root}}/bin"
```

### Python venv not activating

```toml
# Ensure setting is enabled
[settings]
python_venv_auto_create = true

# And env is configured
[env]
_.python.venv = { path = ".venv", create = true }
```

## Security Notes

### Sensitive Data Handling

```toml
# ❌ NEVER do this
[env]
DATABASE_PASSWORD = "mypassword"
API_KEY = "sk-1234567890"

# ✅ Reference from system environment
[env]
DATABASE_PASSWORD = "{{env.DATABASE_PASSWORD}}"
API_KEY = "{{env.API_KEY}}"
```

### Secrets Management

mise only references system environment variables via `{{env.VAR}}` syntax. It does not provide secret storage.

```toml
# mise.toml - references system environment
[env]
DATABASE_URL = "{{env.DATABASE_URL}}"
API_KEY = "{{env.API_KEY}}"
```

For secret storage, use external tools like:
- System environment variables
- `.env` files (gitignored, loaded by your shell)
- `mise.local.toml` (gitignored)
