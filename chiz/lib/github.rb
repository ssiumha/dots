module Lib
  class GithubChiz < Base
    desc 'vscode', 'open online vscode'
    def vscode
      puts <<~END
        # https://github.com/github/dev
        #   - https://docs.github.com/en/codespaces/the-githubdev-web-based-editor
        #   - github.dev로 online vscode를 열 수 있다
        #   - 아무 저장소, pull request에서 `.`을 누르면 들어가진다
        #   - terminal 환경이 필요하면 codespcae를 사용해야 한다
      END
    end
  end
end
