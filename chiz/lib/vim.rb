module Lib
  class VimChiz < Base
    md :global_cmd, 'g///', <<~MD
      # range query
      :g/pattern1/,/pattern2/cmd

      # { 다음줄 ~  } 이전줄 사이에 있는 block을 대상으로 정렬
      :g/{/+1,/}/-1 sort
    MD
  end
end
