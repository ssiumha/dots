# Tools Section

The `[tools]` section declares which tools and versions your project requires.

## Basic Syntax

```toml
[tools]
node = "20"                    # Specific version
python = "3.11"                # Specific version
ruby = "latest"                # Always use latest
go = "1.21.5"                  # Exact version
```

## Version Specifications

| Format | Example | Description |
|--------|---------|-------------|
| Exact version | `"20.0.0"` | Pin to exact version |
| Major version | `"20"` | Latest 20.x.x |
| Latest | `"latest"` | Always use latest stable |
| Prefix match | `"3.11"` | Latest 3.11.x |
| Version file | `".nvmrc"` | Read from file |

## Common Tools

### JavaScript/TypeScript

```toml
[tools]
node = "20"                    # Node.js
bun = "latest"                 # Bun runtime
deno = "latest"                # Deno runtime
pnpm = "latest"                # pnpm package manager
yarn = "latest"                # Yarn package manager
```

### Python

```toml
[tools]
python = "3.11"                # Python runtime
pipx = "latest"                # pipx for global tools
poetry = "latest"              # Poetry dependency manager
uv = "latest"                  # Fast Python package installer
```

### Ruby

```toml
[tools]
ruby = "3.2"                   # Ruby runtime
```

### Go

```toml
[tools]
go = "1.21"                    # Go compiler
```

### Rust

```toml
[tools]
rust = "latest"                # Rust toolchain
```

### Other Languages

```toml
[tools]
java = "21"                    # Java JDK
erlang = "26"                  # Erlang/OTP
elixir = "1.15"                # Elixir
zig = "latest"                 # Zig compiler
```

## Tool Sources

mise supports multiple tool sources beyond the built-in registry:

### npm Packages

```toml
[tools]
"npm:@vscode/dev-container-cli" = "latest"
"npm:typescript" = "5.3"
```

### Cargo (Rust) Crates

```toml
[tools]
"cargo:jwt-cli" = "latest"
"cargo:ripgrep" = "latest"
```

### ubi (Universal Binary Installer)

For GitHub releases:

```toml
[tools]
"ubi:DarthSim/overmind" = "latest"
"ubi:junegunn/fzf" = "latest"
```

### aqua

For aqua-managed tools:

```toml
[tools]
"aqua:aws/copilot-cli" = "latest"
```

### gem (Ruby gems)

```toml
[tools]
"gem:jekyll" = "latest"
```

## Advanced Options

### OS-Specific Versions

```toml
[tools]
node = { version = "20", os = "linux" }
node = { version = "20.1", os = "darwin" }
```

### Install Environment

Custom environment for installation:

```toml
[tools]
python = { version = "3.11", install_env = { PYTHON_CONFIGURE_OPTS = "--enable-shared" } }
```

### Post-Install Hook

Run command after installation:

```toml
[tools]
python = { version = "3.11", postinstall = "pip install -U pip setuptools wheel" }
```

### Custom Executable Name

When the binary name differs from the tool name:

```toml
[tools]
tv = { version = "latest", exe = "television" }
```

## Tool Aliases

Define aliases in the `[alias]` section to use custom sources by default:

```toml
[alias]
sq = "ubi:neilotoole/sq"
xq = "ubi:sibprogrammer/xq"
claude = "npm:@anthropic-ai/claude-code"
```

Then use them like:

```toml
[tools]
sq = "latest"
xq = "latest"
claude = "0.1.0"
```

**Scope**: The `[alias]` section is typically used in global mise configuration (`~/.config/mise/config.toml`). For project-local configurations, it's recommended to use the full prefix directly in `[tools]` for better clarity and portability.

## Examples

### Full-Stack Web App

```toml
[tools]
node = "20"
python = "3.11"
terraform = "latest"
"npm:@devcontainers/cli" = "latest"

[alias]
devcontainer = "npm:@devcontainers/cli"
```

### DevOps/Infrastructure

```toml
[tools]
terraform = "latest"
kubectl = "latest"
helm = "latest"
awscli = "latest"
"ubi:derailed/k9s" = "latest"
"cargo:kdash" = "latest"
```

### Data Engineering

```toml
[tools]
python = "3.11"
"ubi:neilotoole/sq" = "latest"           # SQL query tool
jq = "latest"                             # JSON processor
yq = "latest"                             # YAML processor
"ubi:sibprogrammer/xq" = "latest"         # XML processor
```

## Best Practices

1. **Pin major versions**: Use `"20"` instead of `"latest"` for stability
2. **Use latest for tools**: CLI tools like `jq`, `fzf` can safely use `"latest"`
3. **Document breaking changes**: Add comments for non-obvious version choices
4. **Global vs local**: Install frequently-used tools globally, project-specific ones locally
5. **Check availability**: Not all tools are in mise registry - use prefixes (`npm:`, `ubi:`, etc.)

## Troubleshooting

### Tool not found

```toml
# ❌ This fails if "mycli" isn't in mise registry
mycli = "latest"

# ✅ Use explicit source
"ubi:myorg/mycli" = "latest"
"npm:mycli" = "latest"
```

### Version conflicts

```toml
# ❌ Conflicting versions
node = "18"
node = "20"  # Overrides previous

# ✅ Use environment-specific configs
# mise.toml:       node = "20"
# mise.local.toml: node = "18"  (for local testing)
```

### Installation failures

Check logs with:
```bash
mise doctor
mise install --verbose <tool>
```

Common fixes:
- Add `install_env` with required flags
- Use `postinstall` to fix permissions or symlinks
