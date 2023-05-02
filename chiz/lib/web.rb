module Lib
  class WebChiz < Base
    md :chrome_unsafe, 'https, unsafe, hidden, ERR_CERT_INVALID', <<~MD
      https://yuki.world/skip-chrome-cert-invalid-error/

      chrome에서 인증서가 잘못된 사이트에 접근할 때
      `thisisunsafe` 타이핑하고 엔터를 치면 사이트로 넘어갈 수 있다
    MD

    md :cors, 'cross origin resource sharing', <<~MD
      Origin

      protocol, host, port가 전부 일치하면 같은 Origin으로 판단

      http://example.com != https://example.com
      http://example.com:80 != http://example.com:99
      http://example.com != http://ww.example.com

      CORS json 설정 기본
      ```json
      [
        {
          "AllowedHeaders": [ "*" ],
          "AllowedMethods": [ "GET" ],
          "AllowedOrigins": [ "*" ],
          "ExposeHeaders" [],
          "MaxAgeSeconds": 3000
        }
      ]
      ```
    MD

    md :redirect, 'permanent vs temporary', <<~MD
      permanent -> 301
        - 컨텐츠를 새 도메인으로 영구적으로 마이그레이션
        - http -> https
        - no www -> www
        - URL이 완전히 변경되었다는 표시

      temporary -> 302
        - AB Test
        - 언어, 위치 기반 리다이렉션
        - js 레벨의 리다이렉션

      검색엔진에 영향을 끼친다
        301 permanent일 경우, 수정하게 되면 검색결과도 목표 주소만 사용하도록 갱신되지만
        302는 검색결과에 영향을 끼치지 않는다
    MD

    md :uri, 'url vs uri', <<~MD
      URI: uniform resource identifier
        특정 리소스를 식별하기 위한 고유한 문자열

        `https://example.com/user/10`
          /user 까지는 리소스 위치
          /10 부분을 포함하면 리소스 식별자
          로 볼 수 있으며 URL + URI라 생각할 수 있다

      URI의 format:
        `scheme:[//[user[:password]@]host[:port]][/path][?query][#fragment]`

      URL: uniform resource locator
        URI의 subset이며 리소스의 '실제 위치'를 제시한다
        `https://example.com/index.html` 는 index.html 실제 위치를 표시하므로 URL
    MD
  end
end

