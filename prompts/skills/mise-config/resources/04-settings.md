# Settings Section

The `[settings]` section configures mise's behavior.

## Common Settings

### auto_install

Automatically install missing tools when entering a directory.

```toml
[settings]
auto_install = true  # Default: true
```

When `true`:
- mise installs missing tools from `[tools]` automatically
- No need to run `mise install` manually

When `false`:
- Tools must be installed manually with `mise install`
- Useful for CI or controlled environments

### experimental

Enable experimental features.

```toml
[settings]
experimental = true  # Default: false
```

Current experimental features:
- Python virtualenv auto-activation
- Improved task dependency resolution
- Advanced caching mechanisms

### verbose

Enable verbose output.

```toml
[settings]
verbose = true  # Default: false
```

Shows:
- Tool installation progress
- Task execution details
- Environment variable resolution

### jobs

Number of parallel jobs for installations.

```toml
[settings]
jobs = 8  # Default: 8
```

Higher values speed up multi-tool installations but use more resources.

### legacy_version_file

Read version files from other version managers.

```toml
[settings]
legacy_version_file = true  # Default: true
```

Supported files:
- `.nvmrc` (nvm)
- `.node-version` (nodenv)
- `.ruby-version` (rbenv)
- `.python-version` (pyenv)
- `.go-version` (goenv)

### legacy_version_file_disable_tools

Disable legacy version files for specific tools.

```toml
[settings]
legacy_version_file_disable_tools = ["python", "node"]
```

Useful when you want mise to ignore `.python-version` but still read `mise.toml`.

### always_keep_download

Keep downloaded tool archives.

```toml
[settings]
always_keep_download = true  # Default: false
```

When `true`:
- Archives cached for reinstalls
- Uses more disk space
- Faster reinstalls without re-downloading

### always_keep_install

Keep tool installations even when not in use.

```toml
[settings]
always_keep_install = true  # Default: false
```

Prevents automatic cleanup of unused tool versions.

### plugin_autoupdate_last_check_duration

How often to check for plugin updates.

```toml
[settings]
plugin_autoupdate_last_check_duration = "7d"  # Default: "7d"
```

Formats: `"1d"`, `"12h"`, `"30m"`

## Language-Specific Settings

### Python

#### python_venv_auto_create

Auto-create Python virtual environments.

```toml
[settings]
python_venv_auto_create = true  # Default: false
```

When combined with:

```toml
[env]
_.python.venv = { path = ".venv", create = true }
```

mise will:
1. Create `.venv` if missing
2. Activate it when entering the directory
3. Deactivate when leaving

#### python_default_packages_file

Install packages when creating a new Python environment.

```toml
[settings]
python_default_packages_file = "requirements.txt"
```

Automatically runs `pip install -r requirements.txt` after creating venv.

#### python_compile

Compile Python to bytecode on installation.

```toml
[settings]
python_compile = true  # Default: false
```

Speeds up startup time but increases installation time.

### Node.js

#### node_corepack

Enable Node.js corepack.

```toml
[settings]
node_corepack = true  # Default: false
```

Enables package managers defined in `package.json`:

```json
{
  "packageManager": "pnpm@8.0.0"
}
```

### Go

#### go_default_packages_file

Install Go packages when Go is installed.

```toml
[settings]
go_default_packages_file = "tools.go"
```

### Ruby

#### ruby_install_repo

Custom ruby-install repository.

```toml
[settings]
ruby_install_repo = "https://github.com/custom/ruby-install"
```

#### ruby_build_repo

Custom ruby-build repository.

```toml
[settings]
ruby_build_repo = "https://github.com/custom/ruby-build"
```

## CI/CD Settings

### disable_tools

Disable automatic tool installation for specific tools.

```toml
[settings]
disable_tools = ["python", "node"]
```

Useful in CI where tools are pre-installed.

### ci

Set mise to CI mode.

```toml
[settings]
ci = true  # Default: false
```

In CI mode:
- Disables interactive prompts
- Uses less colorful output
- Skips unnecessary checks

## Task Settings

### task_output

Control task output display.

```toml
[settings]
task_output = "prefix"  # Options: "prefix", "interleave", "hide"
```

- `"prefix"`: Prefix each line with task name
- `"interleave"`: Show output as it happens
- `"hide"`: Suppress output (errors still shown)

### raw

Show raw task output without processing.

```toml
[settings]
raw = true  # Default: false
```

Disables:
- Output prefixing
- Color manipulation
- Progress indicators

## Security Settings

### trusted_config_paths

Directories where config is trusted without prompting.

```toml
[settings]
trusted_config_paths = ["/home/user/projects"]
```

Config files in these paths don't require `mise trust`.

### paranoid

Extra security checks.

```toml
[settings]
paranoid = true  # Default: false
```

Enables:
- Stricter config validation
- Additional permission checks
- Warning on potentially unsafe operations

## Examples

### Development Environment

```toml
[settings]
auto_install = true
experimental = true
verbose = false
python_venv_auto_create = true
node_corepack = true
```

### CI/CD Environment

```toml
[settings]
auto_install = true
ci = true
verbose = true
jobs = 4
task_output = "prefix"
always_keep_download = false
```

### Production-Ready Project

```toml
[settings]
auto_install = false              # Manual control
experimental = false              # Stability
python_venv_auto_create = true    # Convenience
always_keep_install = true        # Avoid re-downloads
paranoid = true                   # Security
```

### Minimal Config

Most projects can use defaults and only set a few options:

```toml
[settings]
python_venv_auto_create = true
```

## Idiomatic Version Files

### idiomatic_version_file

Generate idiomatic version files (`.node-version`, `.ruby-version`, etc.) from mise.toml.

```toml
[settings]
idiomatic_version_file = true  # Default: false
```

When enabled, mise creates `.node-version` when you have:

```toml
[tools]
node = "20"
```

### idiomatic_version_file_enable_tools

Only generate idiomatic files for specific tools.

```toml
[settings]
idiomatic_version_file_enable_tools = ["ruby", "node"]
```

Useful when some tools should use mise-only, others should be compatible with other managers.

## Complete Example

```toml
[settings]
# Installation behavior
auto_install = true
jobs = 8
always_keep_download = false
always_keep_install = true

# Features
experimental = true
verbose = false

# Python
python_venv_auto_create = true
python_default_packages_file = "requirements.txt"

# Node.js
node_corepack = true

# Version file compatibility
legacy_version_file = true
idiomatic_version_file_enable_tools = ["ruby"]

# Tasks
task_output = "prefix"

# Security
trusted_config_paths = ["/home/user/workspace"]
paranoid = false
```

## Best Practices

1. **Start minimal**: Only set what you need
2. **Enable experimental carefully**: May change between releases
3. **Use auto_install in dev**: Convenience for developers
4. **Disable auto_install in CI**: Explicit control
5. **Set python_venv_auto_create**: Avoids common Python issues
6. **Use idiomatic files sparingly**: Only if team uses multiple version managers

## Troubleshooting

### Tools not auto-installing

Check:
```toml
[settings]
auto_install = true  # Must be true
```

If still failing:
```bash
mise install --verbose  # See installation details
mise doctor             # Check for issues
```

### Virtual environment not activating

Ensure both settings are configured:

```toml
[settings]
python_venv_auto_create = true

[env]
_.python.venv = { path = ".venv", create = true }
```

Note: Older mise versions (< v2024.1.0) required `experimental = true`. Recent versions enable venv automatically.

### Tasks not showing output

Check:
```toml
[settings]
task_output = "interleave"  # or "prefix"
raw = false
```

### Config not trusted

Run:
```bash
mise trust
```

Or add to trusted paths:
```toml
[settings]
trusted_config_paths = ["/path/to/project"]
```
