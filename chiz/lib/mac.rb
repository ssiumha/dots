module Lib
  class MacChiz < Base
    md 'sudo_touch', 'use touch id instead password when sudo', <<~MD
      # /etc/pam.d/sudo 파일 둘째줄에 추가
      auth sufficient pam_tid.so
    MD

    desc 'emoji', 'how to input emoji'
    def emoji
      puts <<~END
        CTRL + CMD + SPACE 🤔
      END
    end
  end
end
