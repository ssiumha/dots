module Lib
  class GitChiz < Base
    md 'grep', 'git history grep', <<~MD, lang: :bash
      # 히스토리 전체 패치에서 코드 검색하기
      git log --patch -S <text>
    MD

    md 'chmod', 'update git chmod', <<~MD, lang: :bash
      # https://git-scm.com/docs/git-update-index#Documentation/git-update-index.txt---chmod-x
      git update-index --chmod=+x /path/to/file

      # ref config
      git config core.fileMode
    MD

    md 'show_revision_file', 'show specific revision file', <<~MD, lang: :bash
      # 임의의 revision 파일 확인
      git show <revision|branch|HEAD~xx>:./path/to/file/xxx.rb | bat -lruby

      # 복구시켜서 확인하기
      git restore -s <revision|branch> -- file_path
    MD
  end
end
