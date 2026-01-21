#!/bin/sh
set -e

# Configuration
DOTS_DIR="${DOTS_DIR:-$HOME/dots}"
DOTS_REPO_SSH="git@github.com:ssiumha/dots.git"
DOTS_REPO_HTTPS="https://github.com/ssiumha/dots.git"

# Color codes
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[0;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

# Helper functions
log_info() {
  printf "${BLUE}[INFO]${NC} %s\n" "$1"
}

log_success() {
  printf "${GREEN}[SUCCESS]${NC} %s\n" "$1"
}

log_skip() {
  printf "${YELLOW}[SKIP]${NC} %s\n" "$1"
}

log_error() {
  printf "${RED}[ERROR]${NC} %s\n" "$1"
}

confirm() {
  printf "%s [Y/n] " "$1"
  read -r answer
  case "$answer" in
    [nN]*) return 1 ;;
    *) return 0 ;;
  esac
}

run_rake() {
  task_name="$1"
  log_info "Running rake ${task_name}..."
  cd "$DOTS_DIR" || {
    log_error "Failed to change directory to $DOTS_DIR"
    return 1
  }
  # Ensure Homebrew is in PATH for subprocesses
  if [ -f /opt/homebrew/bin/brew ]; then
    export PATH="/opt/homebrew/bin:$PATH"
  elif [ -f /usr/local/bin/brew ]; then
    export PATH="/usr/local/bin:$PATH"
  fi
  if rake "$task_name"; then
    log_success "Completed: ${task_name}"
  else
    log_error "Failed: ${task_name}"
    return 1
  fi
}

# Main installation process
main() {
  log_info "Starting dots installation..."
  echo ""

  # 1. Xcode Command Line Tools
  if confirm "Install Xcode Command Line Tools?"; then
    if xcode-select -p >/dev/null 2>&1; then
      log_skip "Xcode Command Line Tools already installed"
    else
      log_info "Installing Xcode Command Line Tools..."
      xcode-select --install
      log_info "Please wait for Xcode Command Line Tools installation to complete, then re-run this script"
      exit 0
    fi
  else
    log_skip "Xcode Command Line Tools installation"
  fi
  echo ""

  # 2. Clone dots repository
  if confirm "Clone dots repository to ${DOTS_DIR}?"; then
    if [ -d "$DOTS_DIR" ]; then
      log_skip "dots repository already exists at ${DOTS_DIR}"
    else
      log_info "Cloning dots repository..."
      if git clone "$DOTS_REPO_SSH" "$DOTS_DIR" 2>/dev/null; then
        log_success "Cloned via SSH"
      else
        log_info "SSH clone failed, trying HTTPS..."
        if git clone "$DOTS_REPO_HTTPS" "$DOTS_DIR"; then
          log_success "Cloned via HTTPS"
        else
          log_error "Failed to clone repository"
          exit 1
        fi
      fi
    fi
  else
    log_skip "dots repository cloning"
    if [ ! -d "$DOTS_DIR" ]; then
      log_error "dots directory does not exist at ${DOTS_DIR}. Cannot proceed."
      exit 1
    fi
  fi
  echo ""

  # 3. SSH key generation
  if confirm "Generate SSH key?"; then
    if [ -f "$HOME/.ssh/id_ed25519" ] || [ -f "$HOME/.ssh/id_rsa" ]; then
      log_skip "SSH key already exists"
    else
      run_rake install:ssh_key
    fi
  else
    log_skip "SSH key generation"
  fi
  echo ""

  # 4. Homebrew
  if confirm "Install Homebrew?"; then
    if command -v brew >/dev/null 2>&1; then
      log_skip "Homebrew already installed"
    else
      run_rake install:homebrew
      # Update PATH for current session
      if [ -f /opt/homebrew/bin/brew ]; then
        eval "$(/opt/homebrew/bin/brew shellenv)"
        log_info "Homebrew PATH updated for this session"
      elif [ -f /usr/local/bin/brew ]; then
        eval "$(/usr/local/bin/brew shellenv)"
        log_info "Homebrew PATH updated for this session"
      fi
    fi
  else
    log_skip "Homebrew installation"
  fi
  echo ""

  # 5. mise
  if confirm "Install mise?"; then
    if [ -d "$HOME/.local/share/mise" ]; then
      log_skip "mise already installed"
    else
      run_rake install:mise
    fi
  else
    log_skip "mise installation"
  fi
  echo ""

  # 6. RC files (zshrc, vimrc)
  if confirm "Install RC files (.zshrc, .vimrc)?"; then
    run_rake install:rc
  else
    log_skip "RC files installation"
  fi
  echo ""

  # 7. Config symlinks
  if confirm "Create config symlinks?"; then
    run_rake install:config
  else
    log_skip "Config symlinks creation"
  fi
  echo ""

  # 8. zprofile for mise
  if confirm "Setup zprofile for mise?"; then
    run_rake install:zprofile
  else
    log_skip "zprofile setup"
  fi
  echo ""

  # 9. Brew core packages
  if confirm "Install brew core packages?"; then
    run_rake install:brew:core
  else
    log_skip "brew core packages installation"
  fi
  echo ""

  # 10. Brew dev packages
  if confirm "Install brew dev packages?"; then
    run_rake install:brew:dev
  else
    log_skip "brew dev packages installation"
  fi
  echo ""

  # 11. Brew extra packages
  if confirm "Install brew extra packages?"; then
    run_rake install:brew:extra
  else
    log_skip "brew extra packages installation"
  fi
  echo ""

  # 12. mise tools
  if confirm "Install mise tools?"; then
    if command -v mise >/dev/null 2>&1; then
      log_info "Running mise install..."
      cd "$DOTS_DIR" || {
        log_error "Failed to change directory to $DOTS_DIR"
        return 1
      }
      if mise install; then
        log_success "mise tools installed"
      else
        log_error "mise install failed"
      fi
    else
      log_skip "mise not found, skipping mise install"
    fi
  else
    log_skip "mise tools installation"
  fi
  echo ""

  # 13. Vim plugins
  if confirm "Install vim plugins?"; then
    run_rake install:vim_plugins
  else
    log_skip "vim plugins installation"
  fi
  echo ""

  # 14. Claude Code settings
  if confirm "Install Claude Code settings?"; then
    run_rake install:claude
  else
    log_skip "Claude Code settings installation"
  fi
  echo ""

  # 15. macOS settings
  if confirm "Configure macOS settings?"; then
    run_rake install:macos
  else
    log_skip "macOS settings configuration"
  fi
  echo ""

  log_success "Installation completed!"
  echo ""
  log_info "Next steps:"
  echo "  1. Restart your terminal or run: source ~/.zshrc"
  echo "  2. If you generated a new SSH key, add it to GitHub"
  echo "  3. Enjoy your new development environment!"
}

# Run main function
main
