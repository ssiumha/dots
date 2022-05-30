module Lib
  class JqChiz < Base
    desc 'flatten', 'flatten list items'
    def flatten
      puts <<~END
        cat x.json | jq '.hits.hits[] | ._source.user_ids | flatten | .[]'
      END
    end
  end
end
