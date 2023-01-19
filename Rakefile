# frozen_string_literal: true
# TODO: 호환을 생각해서 ruby 2.6 언저리 문법만 사용
# TODO: system ruby 설치는 sh나 Makefile로 만들어야할까

require 'fileutils'
require 'json'

HOME = ENV['HOME']
DOT_ZSHRC = "#{Dir.home}/.zshrc"
DOT_VIMRC = "#{Dir.home}/.vimrc"
DOT_ASDF = "#{Dir.home}/.asdf"
DOT_VERSIONS = "#{Dir.home}/.tool-versions"

DOTFILES_DIR = File.dirname(__FILE__)
CONFIG_DIR = "#{Dir.home}/.config"
LOCAL_DIR = "#{Dir.home}/.local"

OS_TYPE = case `uname -s`.chop.to_s
          when 'Darwin' then :osx
          when 'Linux' then :linux
          else :unknown
          end


task :default do
  sh 'rake -sP'
end

desc '모든 환경을 무조건 최신화'
task :bootstrap => %i(bootstrap:prepare_dir
                      bootstrap:symlink_config
                      bootstrap:install_rc
                      bootstrap:init_brew
                      ) do
end

namespace :bootstrap do
  task :prepare_dir do
    puts '== RUN PREAPRE DIR =='

    sh %{ mkdir -p #{HOME}/.config }
    sh %{ mkdir -p #{HOME}/.local/{repo,bin,vim,sh,share} }
    sh %{ mkdir -p #{HOME}/.local/vim/tmp/{undo,backup,swap} }
  end

  task :symlink_config do
    puts '== RUN SYMLINK .config =='

    Dir.glob('config/*').each do |config_path|
      name = File.basename config_path
      src_path = File.join(DOTFILES_DIR, config_path)
      dest_path = File.join(CONFIG_DIR, name)

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

  task :install_rc do
    puts '== RUN INSTALL vimrc, zshrc =='

    File.write(DOT_ZSHRC, "source #{DOTFILES_DIR}/zshrc").tap { puts 'created .zshrc' } unless File.exist?(DOT_ZSHRC)
    File.write(DOT_VIMRC, "source #{DOTFILES_DIR}/vimrc").tap { puts 'created .vimrc' } unless File.exist?(DOT_VIMRC)
  end


  task :gitconfig do
    # touch "$$HOME/.gitconfig"; # save to user.name, user.email
  end

  task :init_brew do
    next if OS_TYPE != :osx

    puts '== INSTALL BREW =='

    'brew already installed' unless `command -v brew`.chomp.empty?

    sh %{ /bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)" }

    sh %{ brew bundle }

    # gureumkim; open '/Library/Input Methods/Gureum.app'
    # elixir q git mycli mosh tmux overmind dbvear
  end

  task :install_gh_bin do
    install_github_release 'neovim/neovim', 'v0.8.2'
    install_github_release 'junegunn/fzf', '0.35.1'
    install_github_release 'BurntSushi/ripgrep', '13.0.0'
    install_github_release 'ogham/exa', 'v0.10.1'
    install_github_release 'dandavison/delta', '0.15.1'
    install_github_release 'Wilfred/difftastic', '0.41.0'
    install_github_release 'ajeetdsouza/zoxide', 'v0.9.0'
    install_github_release 'denoland/deno', 'v1.29.2'
    install_github_release 'sharkdp/fd', 'v8.6.0'
    install_github_release 'sharkdp/bat', 'v0.22.1'
    install_github_release 'bootandy/dust', 'v0.8.3'

    # TODO jq, yq?
  end

  # https://github.com/neovim/neovim/tags
  # https://github.com/neovim/neovim/releases/latest
  # https://github.com/neovim/neovim/releases/tag/stable
  # https://github.com/neovim/neovim/releases/expanded_assets/v0.8.2
  def install_github_release(repo_path, tag, bin_alias: nil)
    require 'net/http'
    require 'uri'
    require 'fileutils'

    cache_dir = "#{Dir.home}/.cache/dotfiles/gh/downloads/#{repo_path.gsub('/', '__')}"

    sh %{ mkdir -p #{cache_dir} }

    # Download Github Release
    res = Net::HTTP.get_response URI("https://github.com/#{repo_path}/releases/expanded_assets/#{tag}")
    download_paths = res.body.split("\n").grep(/href/).grep_v(/sha256sum|deb|msi/).map { /href="(?<href>.+?)"/ =~ _1; href }
    download_path = pick_download_path(download_paths)

    binding.irb and raise "not found download_path: #{repo_path}" if download_path.nil?

    download_filename = download_path.split('/').last
    filename = "#{tag}_#{download_filename}"

    if File.exist?("#{cache_dir}/#{filename}")
      puts 'skip. file already exist'
    else
      sh %{ curl -L -o "#{cache_dir}/#{filename}" "https://github.com/#{download_path}" }
    end

    # Unpack Release
    Dir.chdir(cache_dir) do
      sh %{ rm -rf #{tag}; mkdir #{tag}; }

      case download_filename
      when /tar\.gz/ then sh %{ tar -xf #{filename} -C ./#{tag} }
      when /.zip/    then sh %{ unzip #{filename} -d ./#{tag} }
      else
      end
    end

    # Install Release
    bins = Dir.glob("#{cache_dir}/#{tag}/**/*").select { |path| File.stat(path).mode.to_s(8).match('100755') }
    bins = bins.grep /bin/ if bins.count > 1

    bins.each do |binpath|
      binname = bin_alias || File.basename(binpath)
      FileUtils.rm "#{Dir.home}/.local/bin/#{binname}", force: true
      FileUtils.ln_s(binpath, "#{Dir.home}/.local/bin/#{binname}")
    end
  end

  # TODO linux case
  def pick_download_path(download_paths)
    download_paths = case `uname -s`.chop
                     when /darwin/i
                       download_paths.grep(/darwin|apple|mac/)
                     else
                       download_paths.grep(/linux/)
                     end

    return download_paths.first if download_paths.count == 1

    download_paths = case `uname -m`.chop
                     when /x86_64/
                       download_paths.grep(/amd64|x86_64/)
                     when /arm64/
                       download_paths.grep(/arm64/)
                     else
                     end

    download_paths.first
  end

  # task :install_tmux do
  #   install tmux dependencies OR zellij
  ##   sudo apt install autotools-dev automake pkg-config libevent-dev ncurses-dev byacc
  # end
  #
  # TODO: tmux, git은 brew 쓰는걸로. ruby도 brew로?
  #
  # TODO: zshrc 쪽 PATH 제대로 조정해놓기..
  #   .asdf ; .local/bin ; dotfiles/bin ; brew/bin ; /usr/bin 순으로..
  # TODO asdf plugin 사용 목록
  #    - lang: python, perl, poetry, ruby, nodejs
  #    - infra: terraform, awscli, kubectl, eksctl, helm, k9s
  #    - ide: pre-commit, direnv
  #    - utils: yq, jq
  #
  # task :install_asdf do
  #   unless File.exist?(DOT_ASDF)
  #     system "git clone https://github.com/asdf-vm/asdf.git #{DOT_ASDF}"
  #     Dir.chdir DOT_ASDF do
  #       system 'git checkout $(git describe --abbrev=0 --tags)'
  #     end
  #   end
  #
  #   asdf plugin add #{plugin_name}
  #   asdf install #{plugin_name}
  #
  #	  asdf direnv setup --shell zsh --version latest
  #   TODO dotfiles/asdf 랑 dockerfile 정리 필요
  # end
end


namespace :custom do
  # install hidetab - 하면 편하긴 하다
  #
  # define _CSS_BODY
  # #main-window[tabsintitlebar="true"]:not([extradragspace="true"]) #TabsToolbar {
  #   opacity: 0;
  #   pointer-events: none;
  # }
  #
  # #main-window:not([tabsintitlebar="true"]) #TabsToolbar {
  #   visibility: collapse !important;
  # }
  # endef
  # opt-firefox-hidetab: export CSS_BODY:=$(_CSS_BODY)
  # opt-firefox-hidetab:
  # 	cd "$$(find "$$HOME/Library/Application Support/Firefox/Profiles/" | egrep 'default-release$$' | head -n1)" \
  # 	&& if [[ $$(pwd) =~ "default-release" ]]; \
  # 	then \
  # 		pwd; \
  # 		mkdir -p chrome; \
  # 		echo "$$CSS_BODY" > chrome/UserChrome.css; \
  # 	else \
  # 		echo 'opt-firefox-hidetab: failed cd'; \
  # 	fi
  #
  # opt-firefox-hidetab-remove:
  # 	cd "$$(find "$$HOME/Library/Application Support/Firefox/Profiles/" | egrep 'default-release$$' | head -n1)" \
  # 	&& if [[ $$(pwd) =~ "default-release" ]]; \
  # 	then \
  # 		pwd; \
  # 		mv chrome/UserChrome{,_}.css; \
  # 	else \
  # 		echo 'opt-firefox-hidetab: not found UserChrome.css'; \
  # 	fi
end


namespace :docker do
  task :build do
    sh %{ docker buildx build --progress=plain -f docker/Dockerfile -t home . }
  end

  task :run do
    sh <<~SH
      docker run -it --rm \
      	--name home \
      	--hostname home-docker \
      	-v /var/run/docker.sock:/var/run/docker.sock \
      	-v $$HOME/dotfiles:/home/me/dotfiles \
      	-v $$HOME/.ssh:/home/me/.ssh \
      	-e HOST_HOME=$$HOME \
      	-e HOST_USER_ID=$$(id -u $$USER) \
      	-e HOST_GROUP_ID=$$(id -g $$USER) \
      	home:latest
    SH
  end

  task :run_root do
    sh <<~SH
      docker run -it --rm \
      	--name home \
      	-v /var/run/docker.sock:/var/run/docker.sock \
      	home:latest root

    SH
  end
end

namespace :util do
  desc '새 PC로 작업 환경 이전'
  task :migrate_pc do
    puts <<~LIST
      # home
      .ssh                - move all files
      scripts             - move all files
      .local/zsh/history  - need?
      .local/zsh/zoxide   - need?

      .zshrc      - check
      .config     - confirm modify
      dotfiles    - confirm modify
      repos       - confirm modify
                  - check all .envrc, .tool-versions files
    LIST

    # TODO rsync .git 포함해서 싹다 특정 ssh로 보내버리기
  end

  task :gen_key do
    # TODO create ssh key
    # TODO gitssh key -> ssh git@github.com
  end
end
