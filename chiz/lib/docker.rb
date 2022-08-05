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
  end
end
