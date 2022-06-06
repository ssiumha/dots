module Lib
  class BashChiz < Base
    desc 'ps', 'process list'
    def ps
      puts doc(<<~END)
        # 프로세스 목록 + 사용중인 환경 변수
        ps eww
      END
    end

    desc 'date', 'format date'
    def date
      puts doc(<<~END)
        # GNU date
        date +'%Y-%m-%d %H:%M:%S'
      END
    end

    desc 'if_interactive', 'check shell is interactive mode'
    def if_interactive
      puts doc(<<~END)
        # check shell is interactive mode
        [[ $- == *i* ]] && echo 'interactive mode!'
        [[ -o interactive ]] && echo 'interactive mode!'
      END
    end

    desc 'if', 'bash if methods'
    def if
      puts doc(<<~END)
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
        [ -z $STR ] # str.length == 0
        [ -n $STR ] # str.length != 0
        [ -d $PATH ] # path.is_directory?
        [ -f $PATH ] # !path.is_directory?
        [ -e $PATH ] # path.exist_file?
        [ -L $PATH ] # path.symbolic_link?
        [ -s $PATH ] # path.file_size > 0
        [ -x $PATH ] # path.executable?
      END
    end
  end
end
