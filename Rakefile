# frozen_string_literal: true

require 'fileutils'

DOT_DIR = File.dirname(__FILE__)
DOT_ZSHRC = "#{Dir.home}/.zshrc"
DOT_VIMRC = "#{Dir.home}/.vimrc"
DOT_CONFIG = "#{Dir.home}/.config"
DOT_CACHE = "#{Dir.home}/.cache"
DOT_MISE = "#{Dir.home}/.local/share/mise"

# defaults write -g ApplePressAndHoldEnabled -bool false
# defaults -currentHost write -g AppleFontSmoothing -int 1
#   - defaults write -g CGFontRenderingFontSmoothingDisabled -bool YES

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
task 'install:all' => ['install:base', 'install:config', 'install:brew', 'install:mise'] do
end

desc 'install zshrc, vimrc'
task 'install:rc' do
  File.write(DOT_ZSHRC, "source #{DOT_DIR}/zshrc").tap { puts 'created .zshrc' } unless File.exist?(DOT_ZSHRC)
  File.write(DOT_VIMRC, "source #{DOT_DIR}/vimrc").tap { puts 'created .vimrc' } unless File.exist?(DOT_VIMRC)

  FileUtils.mkdir_p File.join(DOT_CACHE, 'vim/undo')
  FileUtils.mkdir_p File.join(DOT_CACHE, 'vim/swap')
  FileUtils.mkdir_p File.join(DOT_CACHE, 'vim/backup')
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

desc 'remove symlinked configs'
task 'uninstall:config' do
  Dir.glob('config/*').each do |config_path|
    name = File.basename(config_path)
    dest_path = File.join(DOT_CONFIG, name)

    pname = name.ljust(10)

    if File.symlink? dest_path
      FileUtils.rm dest_path
      puts "#{pname} : removed"
    else
      puts "#{pname} : not linked"
    end
  end
end

desc 'init brew'
task 'install:brew' do
  next if OS_TYPE != :osx

  if `command -v brew`.chomp.empty?
    sh %{ /bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)" }
  end

  sh <<~SH
    cat Brewfile | brew bundle --file=/dev/stdin
  SH
end

desc 'install mise'
task 'install:mise' do
  return puts 'already installed mise' if File.exist?(DOT_MISE)

  sh 'curl https://mise.run | sh'
end

desc 'install vscode settings'
task 'install:vscode' do
  VSCODE_SETTINGS_PATH = "#{Dir.home}/Library/Application Support/Code/User/settings.json"
  VSCODE_KEYBINDING_PATH = "#{Dir.home}/Library/Application Support/Code/User/keybindings.json"

  if File.symlink? VSCODE_SETTINGS_PATH
    puts "vscode settings.json : already linked"
  elsif File.exist? VSCODE_SETTINGS_PATH
    puts "vscode settings.json : link failed. already exist file"
    puts "should execute `rm '#{VSCODE_SETTINGS_PATH}'`"
  else
    FileUtils.ln_s File.join(DOT_CONFIG, 'vscode/settings.json'), VSCODE_SETTINGS_PATH
    FileUtils.ln_s File.join(DOT_CONFIG, 'vscode/keybindings.json'), VSCODE_KEYBINDING_PATH
    puts "vscode settings.json : now linked"
    puts "vscode keybindings.json : now linked"
  end

  # TODO
  # code --list-extensions > code_extensions
  # code --install-extension vscodevim.vim
  #   aykutsarac.jsoncrack-vscode
  #   vscodevim.vim
  #   github.copilot
  #   github.copilot-chat
end
