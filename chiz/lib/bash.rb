module Lib
  class BashChiz < Base
    md :makefile_template, 'easy makefile', <<~MD, lang: :makefile
      # REF: space -> tab 으로 작성해야한다
      .EXPORT_ALL_VARIABLES:

      .PHONY: help test
      .DEFAULT_GOAL := help

      SHELL := /usr/bin/env bash
      SHELL_OPTIONS ?= -euo pipefail

      help:
        @awk 'BEGIN {FS = ":.*?##"} \
          /^[^_.#][a-zA-Z0-9_-]+:($$| .*?)/ {printf "\033[36m%-30s\033[0m %s\n", $$1, $$2}' \
          $(MAKEFILE_LIST) \
          | sort

      link-config: CONFIG_PATH=$$HOME
      link-config:
        touch "${CONFIG_PATH}/test.txt"
        if [[ $$(pwd) =~ "dotfiles" ]]; \
          then echo "current path is dotfiles"; \
          else echo "current path is not odtfiles"; \
        fi
    MD

    md :ansi_color, 'echo color', <<~MD
      Black        0;30     Dark Gray     1;30
      Red          0;31     Light Red     1;31
      Green        0;32     Light Green   1;32
      Brown/Orange 0;33     Yellow        1;33
      Blue         0;34     Light Blue    1;34
      Purple       0;35     Light Purple  1;35
      Cyan         0;36     Light Cyan    1;36
      Light Gray   0;37     White         1;37

      ```bash
        echo -e "\\033[0;31m RED COLOR TEXT \\033[0m"
      ```
    MD

    md :asdf, 'asdf cheatsheet', <<~MD, lang: :bash
      # ruby-build 할 떄 매번 openssl 설치 안하고 실행하기
      # disable-shared: libruby.3.1.dylib (no such file) 에러 나서 추가
      brew install openssl@1.1
      export RUBY_CONFIGURE_OPTS="--disable-install-doc --disable-shared --with-openssl-dir=$(brew --prefix openssl@1.1)"
      asdf install ruby 3.1.2
    MD

    md :file_replace, 'replace perl, ruby, sed', <<~MD, lang: :bash
      # sed는 BSD, GNU 인자도 다르고 정규식 규칙도 조금 달라서 별로..
      # perl: 단순 치환시 유용. sed 거의 대체 가능
      # ruby: hash, array 등 자료구조 들어가면 ruby 아니면 awk로..

      # -i 뒤에 인자 없으면 그대로 덮어쓰기
      ruby -i'.bak' -pe '$_.gsub! /namespace/, %(name); puts $_' file.txt
      perl -i -pe 's/namespace/name/;' file.txt

      # e: ExMode / s: Silent / n: No Swap. memory only / N: nocompatible
      vim -Nens +'g/namespace/norm nciwname' +wq file.txt
      vim -Nens +'bufdo g/namespace/norm nciwname' +wqa file.txt file2.txt
    MD

    md :perl, 'perl tip tircks', <<~MD, lang: :perl
      # arguments
      perl -ane 'print @F[0]'
    MD

    md :ps, 'process list', <<~MD
      # 프로세스 목록 + 사용중인 환경 변수
      ps eww
    MD

    md :date, 'format date', <<~MD
      # GNU date
      date +'%Y-%m-%d %H:%M:%S'
    MD

    md :argument, 'use argument in method, script file', <<~MD
      if [ $# -eq 0 ]  # check argument count
    MD

    md :case, 'case statement', <<~MD, lang: :bash
      case "production" in
        production)
          export NAMESPACE='production'
          echo 'production case..'
          ;;

        staging) export NAMESPACE='staging' ;;

        *)
          exit 1
          ;;
      esac
    MD

    md :if_interactive, 'check shell is interactive mode', <<~MD
      # check shell is interactive mode
      [[ $- == *i* ]] && echo 'interactive mode!'
      [[ -o interactive ]] && echo 'interactive mode!'
    MD

    md :if, 'bash if methods', <<~MD
      # [ vs [[
        - [ : builtin command. `/bin/[`
          - [ ] 사이의 모든 값은 인자로써 적용
          - test 커맨드의 alias라고 생각해도 된다
        - [[ : bash shell 문법

      # check exit code
      if [ $? -ne 0 ]; then
        echo 'run failed'
      fi

      if ! run command; then
        echo 'run failed'
      fi

      # check command exist
      ## command, type - builtin 기능. POSIX 호환적
      ## which - /usr/bin/which 기능
      test -x <path>
      command -v <command> &>/dev/null
      type <command> &>/dev/null
      which <command> &>/dev/null

      # condition
      [ $STR ] # defined? str
      [ -z $STR ] # str.length == 0
      [ -n $STR ] # str.length != 0
      [ -d $PATH ] # path.is_directory?
      [ -f $PATH ] # !path.is_directory?
      [ -e $PATH ] # path.exist_file?
      [ -L $PATH ] # path.symbolic_link?
      [ -s $PATH ] # path.file_size > 0
      [ -x $PATH ] # path.executable?
    MD
  end
end
