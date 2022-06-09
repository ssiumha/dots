module Lib
  class GitChiz < Base
    desc 'grep', 'git history grep'
    def grep
      puts_doc <<~END
        # 히스토리 전체 패치에서 코드 검색하기
        git log --patch -S <text>
      END
    end

    desc 'chmod', 'update git chmod'
    def chmod
      puts <<~END
        # https://git-scm.com/docs/git-update-index#Documentation/git-update-index.txt---chmod-x
        git update-index --chmod=+x /path/to/file

        # ref config
        git config core.fileMode
      END
    end
  end
end
