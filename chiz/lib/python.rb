
module Lib
  class PythonChiz < Base
    desc 'poetry', 'python poetry project'
    def poetry
      puts doc(<<~END)
        # direnv
        echo 'layout poetry' >> .envrc
        direnv allow

        # init config
        poetry init
        poetry config cache-dir ./vendor/poetry --local
        poetry config --list

        # env
        poetry env use 3.9
        poetry env list --full-path

        # basic
        poetry install
        poetry run <package-cmd>
        poetry shell
      END
    end
  end
end
