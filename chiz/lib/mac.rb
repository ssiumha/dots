module Lib
  class MacChiz < Base
    desc 'emoji', 'how to input emoji'
    def emoji
      puts <<~END
        CTRL + CMD + SPACE 🤔
      END
    end
  end
end
