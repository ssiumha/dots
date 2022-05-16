module Lib
  class GitChiz < Base
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
