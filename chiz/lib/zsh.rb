module Lib
  class ZshChiz < Base
    desc 'zle', 'zsh line editor memo'
    def zle
      puts doc(<<~END)
        # http://zsh.sourceforge.net/Doc/Release/Zsh-Line-Editor.html
        # http://zsh.sourceforge.net/Doc/Release/Zsh-Line-Editor.html#Zle-Builtins
        # http://zsh.sourceforge.net/Doc/Release/Zsh-Line-Editor.html#Standard-Widgets
      END
    end
  end
end
