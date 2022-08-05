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
  end
end

