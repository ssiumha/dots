module Lib
  class WebChiz < Base
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

