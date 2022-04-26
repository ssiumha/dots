# frozen_string_literal: true

require 'fileutils'
require 'json'

DOT_ZSHRC_PATH = "#{Dir.home}/.zshrc"
DOT_VIMRC_PATH = "#{Dir.home}/.vimrc"

DOTFILES_DIR = File.dirname(__FILE__)
CONFIG_DIR = "#{Dir.home}/.config"
LOCAL_DIR = "#{Dir.home}/.local"
MY_LOCAL_DIRS = %w[
  bin
  emacs/tmp
  vim/tmp/undo vim/tmp/backup vim/tmp/swap
  zsh/completion zsh/zplug
].map { File.join(LOCAL_DIR, _1) }

# ---- tasks ----

task :default do
  system 'rake -sP'
end

task 'init:dir' do
  FileUtils.mkdir_p(MY_LOCAL_DIRS)
  FileUtils.mkdir_p CONFIG_DIR
end

task 'config:status' do
  xdg_config_statuses.map { puts config_status_text _1 }
end

task 'config:install' => ['init:dir'] do
  FileUtils.touch "#{Dir.home}/.gitconfig"
  # TODO: if not git username & useremail, set to..

  txt_col = 76
  xdg_config_statuses.map do |st|
    txt = config_status_text st

    case st[:status]
    when :exist
      txt += ' unknown path'.rjust(txt_col - txt.length).red
    when :linked
      txt += ' already linked'.rjust(txt_col - txt.length).yellow
    else
      txt += ' -> linked'.rjust(txt_col - txt.length).green
      FileUtils.ln_s st[:cfg_path], st[:xdg_path]
    end

    puts txt
  end
end

task 'rc:install' => ['init:dir'] do
  unless File.exist?(DOT_ZSHRC_PATH)
    File.write(DOT_ZSHRC_PATH, "source #{DOTFILES_DIR}/zshrc").tap { puts 'created .zshrc' }
  end
  unless File.exist?(DOT_VIMRC_PATH)
    File.write(DOT_VIMRC_PATH, "source #{DOTFILES_DIR}/vimrc").tap { puts 'created .vimrc' }
  end
end

task 'asdf:install' => ['init:dir'] do
  # inlcude setup direnv
end

task 'util:install' => ['init:dir'] do
  os_arch = `uname -m`.chop # x86_64|aarch64|i686|arm
  os_type = case `uname -s`.chop.to_s
            when 'Darwin' then :osx
            when 'Linux' then :linux
            else :unknown
            end

  filter_proc = case os_type
                when :osx
                  proc { (_1 =~ /(darwin|apple)/) && (_1 =~ /#{os_arch}/) }
                when :linux
                  proc { (_1 =~ /(linux)/) && (_1 =~ /#{os_arch}/) }
                else
                  proc { false }
                end

  # TODO: rg, exa, fd, delta, fzf, zoxide, bat?

  {
    'ajeetdsouza/zoxide' => 'z'
  }.map do |repo_path, binname|
    res = JSON.parse `curl -s https://api.github.com/repos/#{repo_path}/releases/latest`
    pickup = res['assets'].filter { |asset| filter_proc.call(asset['name']) }.first
    basename = File.basename repo_path
    zipname = pickup['name']

    # TODO: ~/.local/downloads, ~/.local/releases + symlink
    `mkdir -p /tmp/dotfiles_works`
    Dir.chdir '/tmp/dotfiles_works' do
      `curl -s -L #{pickup['browser_download_url']} -o ./#{zipname}`

      if zipname.end_with? '.tar.gz'
        `rm -rf extracted && mkdir extracted`
        `gtar -zvxf #{zipname} --directory extracted`
        `cp extracted/#{basename} $HOME/.local/bin/#{binname}`
      elsif zipname.end_with? '.zip'
      else
        puts "unknown archived file : #{zipname}"
      end
    end
  end

  # TODO: require gnuzip, tar, curl
  # TODO: download from github release
end

# install asdf (template)

# TODO
# TEST gitssh key -> ssh git@github.com
#
# install gnucmds? ->
#   findutils, coreutils, gnu-sed, gawk, grep, gnu-tar, iproute2mac
#     xargs, date, sed, awk, egrep, tail, echo, tar
#
#
# install docker, alfred
#
# setup-direnv
# envpkg -> asdf, brew
# asdf << jsonnet
#
# install alacritty
# zplug?
#
# install mycli, k9s, gh?
#
# tmux:deps -> install etc..
#
# opt:firefox:hidetab
# opt:firefox:hidetab:rollback
# opt:guremkim:install

# ---- methods ----

def config_status_text(config_status)
  status = case config_status[:status]
           when :linked then 'o'.green
           when :absent then 'x'.red
           else '?'.yellow
           end
  name = config_status[:name].ljust(10)
  path = config_status[:xdg_path]

  "#{status} #{name} -> #{path}"
end

def file_status(path)
  if !Dir.exist?(path) then :absent
  elsif File.symlink?(path) then :linked
  else :exist
  end
end

def xdg_config_statuses
  Dir.glob('./config/*').map do |config_path|
    name = File.basename(config_path)
    xdg_path = File.join(CONFIG_DIR, name)

    {
      name: name,
      cfg_path: File.expand_path(config_path),
      xdg_path: xdg_path,
      status: file_status(xdg_path)
    }
  end
end

# ---- extends ----

# ext color string
class String
  %i[black red green yellow blue magenta cyan gray].each_with_index do |k, i|
    define_method k, -> { "\e[#{30 + i}m#{self}\e[0m" }
    define_method "bg_#{k}", -> { "\e[#{40 + i}m#{self}\e[0m" }
  end

  {
    bold: -> { "\e[1m#{self}\e[22m" },
    italic: -> { "\e[1m#{self}\e[22m" },
    underline: -> { "\e[1m#{self}\e[22m" },
    blink: -> { "\e[1m#{self}\e[22m" },
    reverse_color: -> { "\e[1m#{self}\e[22m" }
  }.each do
    define_method _1, _2
  end

  def noop
    gsub(/\e\[\d+m/, '')
  end
end
