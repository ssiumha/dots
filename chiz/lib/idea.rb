module Lib
  class IdeaChiz < Base
    md :automation_path, 'ci, cd, etc..', <<~MD
      ref: https://github.com/github/super-linter/tree/main/.automation

      CI, CD, deploy에 관련된 파일은 .automation 폴더를 만들어서 관리하면 이름도 알맞고 깔끔하다
    MD
  end
end
