module Lib
  class WebChiz < Base
    md 'weather', 'get weather', <<~MD
      curl wttr.in/Seoul
    MD

    md 'cloud_ping', 'aws cloud latency grid', <<~MD
      https://www.cloudping.co/grid
    MD
  end
end

