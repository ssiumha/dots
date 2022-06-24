module Lib
  class PythonChiz < Base
    desc 'web_server', 'oneline python webserver'
    def web_server
      puts doc(<<~END)
        # python3
        python -m http.server --bind 0.0.0.0 8000

        # python2
        python -m SimpleHTTPServer
      END
    end

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

        poetry add "pendulum>=2.0.5"
        poetry add pendulum@^2.0.5
        poetry add pendulum@latest
      END
    end
  end
end
