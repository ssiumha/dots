# frozen_string_literal: true

# text to pretty jq
def jq(input)
  IO.popen('jq --color-output', 'r+') do |pipe|
    pipe.puts input
    pipe.close_write
    pipe.readlines
  end
end
