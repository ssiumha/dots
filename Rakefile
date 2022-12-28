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

OS_ARCH = `uname -m`.chop # x86_64|aarch64|i686|arm


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

    Dir.glob('./config/*').each do |config_path|
      name = File.basename config_path
      target_path = File.join(CONFIG_DIR, name)

      pname = name.ljust(10)

      if File.symlink? target_path
        puts "#{pname} : already linked"
      elsif Dir.exist? target_path
        puts "#{pname} : link failed. already exist file"
      else
        FileUtils.ln_s config_path, target_path
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
  end

  # TODO: Brewfile 사용해보기
  # task :install_brew do
  #   bat ctags-univirtial git docker mycli mosh tmux overmind q ripgrep ruby-build rust elixir sqlite
  #   casks - alacritty docker gureumkim ngrok openvpn-connect raycast dbvear
  #   util-install-brew -> docker, alacritty, gureumkim, alfred, mycli, dbeaver

  #   brew cask install gureumkim \
  #       && open '/Library/Input Methods/Gureum.app'

  # install gnucmds? -> rust uutils인가 찾아보기
  #   findutils, coreutils, gnu-sed, gawk, grep, gnu-tar, iproute2mac
  #     xargs, date, sed, awk, egrep, tail, echo, tar
  #
  # # Rust carg-bins 써서 일부 유틸 관리하기?
  #   install rust gnuutils
  #   install cargo-bins
  # end

  # task :install_tmux do
  #   install tmux dependencies OR zellij
  ##   sudo apt install autotools-dev automake pkg-config libevent-dev ncurses-dev byacc
  # end

  # task :install_asdf do
  #   unless File.exist?(DOT_ASDF)
  #     system "git clone https://github.com/asdf-vm/asdf.git #{DOT_ASDF}"
  #     Dir.chdir DOT_ASDF do
  #       system 'git checkout $(git describe --abbrev=0 --tags)'
  #     end
  #   end
  #   TODO: language 계통은 devbox로 이행을 생각하고 있고,
  #         프로젝트 상관없이 써야하는 ide, utils는 cargo-bin 고려중. asdf 안써도 되는건가..?
  #   asdf add plugin
  #      lang - python, perl, poetry, ruby, nodejs
  #      infra - terraform, awscli, kubectl, eksctl, helm, k9s
  #      ide - neovim, tmux, pre-commit, delta
  #      utils - yq, jq, ripgrep, exa, fd, dust, bat
  #      zoxide??
  #      asdf plugin add #{plugin_name}
  #      asdf install #{plugin_name}
  #
  #	  asdf direnv setup --shell zsh --version latest
  #   TODO dotfiles/asdf 랑 dockerfile 정리 필요
  # end

  # task :install_devbox do
  # TODO devbox
  #  - https://www.jetpack.io/devbox/docs/ide_configuration/direnv/
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
