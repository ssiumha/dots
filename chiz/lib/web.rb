module Lib
  class WebChiz < Base
    desc 'weather', 'get weather'
    def wheater
      puts <<~END
        curl wttr.in/Seoul
      END
    end
  end
end

