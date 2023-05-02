module Lib
  class VimChiz < Base
    md :global_cmd, 'g///', <<~MD
      # range query
      :g/pattern1/,/pattern2/cmd

      # { 다음줄 ~  } 이전줄 사이에 있는 block을 대상으로 정렬
      :g/{/+1,/}/-1 sort
    MD

    md :open_stdin, 'with filetype', <<~MD
      cat ... | vim +'setf yaml' -
    MD

    md :cli, 'vim one liner', <<~MD, lang: :bash
      # ex == vim -e
      # + 는 cmd mode, - 는 normal mode
      # +wq!, -ZZ 를 사용하면 파일에 저장하고 종료

      ls | ex -s +%s/s/k/ +%p -cq! /dev/stdin
    MD
  end
end
