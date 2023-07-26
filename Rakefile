# frozen_string_literal: true

require 'fileutils'

DOT_DIR = File.dirname(__FILE__)
DOT_ZSHRC = "#{Dir.home}/.zshrc"
DOT_VIMRC = "#{Dir.home}/.vimrc"
DOT_CONFIG = "#{Dir.home}/.config"
DOT_ASDF = "#{Dir.home}/.asdf"

BIN_VERSIONS = {
  'neovim/neovim'      => 'v0.9.1',
  'junegunn/fzf'       => '0.35.1',
  'BurntSushi/ripgrep' => '13.0.0',
  'ogham/exa'          => 'v0.10.1',
  'dandavison/delta'   => '0.16.5',
  'Wilfred/difftastic' => '0.41.0',
  'ajeetdsouza/zoxide' => 'v0.9.1',
  'denoland/deno'      => 'v1.29.2',
  'sharkdp/fd'         => 'v8.7.0',
  'sharkdp/bat'        => 'v0.22.1',
  'bootandy/dust'      => 'v0.8.3',
}

# defaults write -g ApplePressAndHoldEnabled -bool false
# defaults -currentHost write -g AppleFontSmoothing -int 1
#   - defaults write -g CGFontRenderingFontSmoothingDisabled -bool YES
#
# TODO: softwareupdate --install-rosetta --agree-to-license
# TODO: rsync.., brew install tailscale; sudo tailscaled install-system-daemon
# TODO: hash로 바꾸고, .tool-versions 에 지속적으로 merge 하는 방향으로 수정
# TODO: install:asdf 는 베이스 버전과 플러그인만 보장해주는 느낌으로 사용 (각종 언어 플러그인도 미리 추가 + system으로?)
# TODO: list:bin -> 최신버전 tag를 한번에 보고 싶다..

# 옮길 파일: .tool-versions, .aws. .kube, .ssh, repos, dotfiles, spells, .bundle/config, .zshrc, .gitconfig
#  - Downlaods, Documents?
ASDF_TOOL_VERSIONS = <<~EOF
  # infra
  terraform 1.1.8
  awscli 2.8.5
  kubectl 1.25.3
  telepresence 2.8.5
  eksctl 0.63.0
  helm 3.2.4
  helmfile 0.150.0
  k9s 0.27.4
  cmctl 1.9.1

  # shell
  direnv 2.31.0
  # git 2.39.0 # -> brew
  yq 4.29.2
  jq 1.6

  # external plugins
  lazydocker 0.12 # https://github.com/comdotlinux/asdf-lazydocker.git
EOF

# lazydocker

Brewfile = <<-EOF
  tap "homebrew/cask"

  # TODO: gnu utils?
  # gnu-sed gnu-tar gawk
  # coreutils -> gdate
  # findutils -> gxargs
  # grep -> gegrep
  # iproute2mac -> ip

  brew "tmux"
  brew "universal-ctags"
  brew "sqlite"

  brew "mysql"
  brew "tidy-html5"

  cask "docker"
  # cask "alacritty"
  cask "wezterm"
  cask "openvpn-connect"
  cask "ngrok"
  cask "raycast"
  cask "deepl"
EOF

OS_TYPE = case `uname -s`.chop.to_s
          when 'Darwin' then :osx
          when 'Linux' then :linux
          else :unknown
          end

task :default do
  sh 'rake -T'
end

task :console do
  binding.irb
end

desc 'install all process'
task 'install:all' => ['install:base', 'install:config', 'install:bin', 'install:brew', 'install:asdf'] do
end

desc 'install zshrc, vimrc'
task 'install:base' do
  File.write(DOT_ZSHRC, "source #{DOT_DIR}/zshrc").tap { puts 'created .zshrc' } unless File.exist?(DOT_ZSHRC)
  File.write(DOT_VIMRC, "source #{DOT_DIR}/vimrc").tap { puts 'created .vimrc' } unless File.exist?(DOT_VIMRC)
end

desc 'symlink configs'
task 'install:config' do
  FileUtils.mkdir_p DOT_CONFIG
  Dir.glob('config/*').each do |config_path|
    name = File.basename(config_path)
    src_path = File.join(DOT_DIR, config_path)
    dest_path = File.join(DOT_CONFIG, name)

    pname = name.ljust(10)

    if File.symlink? dest_path
      puts "#{pname} : already linked"
    elsif Dir.exist? dest_path
      puts "#{pname} : link failed. already exist file"
    else
      FileUtils.ln_s src_path, dest_path
      puts "#{pname} : now linked"
    end
  end
end

desc 'install nvim, fzf, rg... to .local/bin'
task 'install:bin' do
  require './rakelib/github_release'

  BIN_VERSIONS.map { |path, tag| GithubRelease.new(path, tag).install }
end

desc 'init brew'
task 'install:brew' do
  next if OS_TYPE != :osx

  if `command -v brew`.chomp.empty?
    sh %{ /bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)" }
  end

  sh <<~SH
    cat <<-Brewfile | brew bundle --file=/dev/stdin
      #{Brewfile}
    Brewfile
  SH
end

desc 'install asdf and setup base utils'
task 'install:asdf' do
  return puts 'already installed asdf' if File.exist?(DOT_ASDF)

  sh "git clone https://github.com/asdf-vm/asdf.git #{DOT_ASDF} --branch v0.12.0"
end
