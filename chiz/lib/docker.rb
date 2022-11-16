module Lib
  class DockerChiz < Base
    md :host, 'get host ip info', <<~MD, lang: :sh
      # 내부에서 접근 가능한 domain
      host.docker.internal

      # container gateway IP 가져오기
      docker inspect -f "{{ .NetworkSettings.Gateway }}" "$CONTAINER_NAME"

      # src_port를 지정하지 않았을 때 (docker ... --publish 61234 ), docker에서 임의로 할당한 포트 가져오기
      docker inspect -f "{{(index (index .NetworkSettings.Ports \\"$PORT/tcp\\") 0).HostPort}}" "$CONTAINER_NAME"
    MD

    md :samples, 'google samples', <<~MD
      ```
      docker run -it --rm -p '8000:8080' gcr.io/google-samples/hello-app:1.0

      curl localhost:8000
        Hello, world!
        Version: 1.0.0
        Hostname: 7079c9725e25
      ```

      OR

      ```
      kubectl create deploy web --image=gcr.io/google-samples/hello-app:1.0
      kubectl expose deploy web --port 8080
      ```

      - 2.0을 붙여서 다른 응답을 받을 수도 있다
    MD

    md :compose, 'docker-compose', <<~MD
      ```yaml
      x-assets: &default-assets
        build: ...
        env_file: ...
        tty: ...
        volumes: ...

      services:
        js:
          <<: *default-assets
          command: 'yarn build --watch'

        css:
          <<: *default-assets
          command: 'yarn build:css --watch'
      ```

      - yaml doc 문법을 사용하면 여러 용도의 docker를 간단하게 만들 수 있다
    MD
  end
end
