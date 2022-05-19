module Lib
  class BashChiz < Base
    desc 'date', 'format date'
    def date
      puts doc(<<~END)
        # GNU date
        date +'%Y-%m-%d %H:%M:%S'
      END
    end

    desc 'if', 'bash if methods'
    def if
      puts doc(<<~END)
        # [ vs [[
          - [ : builtin command. `/bin/[`
            - [ ] 사이의 모든 값은 인자로써 적용
          - [[ : bash shell 문법

        # cehck exit code
        if [ $? -ne 0 ]; then
          echo 'run failed'
        fi

        if ! run command; then
          echo 'run failed'
        fi

        # condition
        [ -z $STR ] # str.length == 0
        [ -n $STR ] # str.length != 0
        [ -d $PATH ] # path.is_directory?
        [ -f $PATH ] # !path.is_directory?
        [ -e $PATH ] # path.exist_file?
        [ -L $PATH ] # path.symbolic_link?
        [ -s $PATH ] # path.file_size > 0
      END
    end
  end
end
