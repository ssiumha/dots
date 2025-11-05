#!/usr/bin/env ruby
# install:
# {
#   "hooks": {
#     "UserPromptSubmit": [
#       { "hooks": [ { "type": "command", "command": "~/dots/prompts/hooks/submit.rb" } ] }
#     ]
#   }
# }

require 'json'
require 'yaml'

# 스크립트 디렉토리 경로
SCRIPT_DIR = File.expand_path('~/dots/prompts/hooks')
RULES_FILE = File.join(SCRIPT_DIR, 'submit-rules.yaml')

begin
  # 입력 JSON 파싱
  input_data = JSON.parse($stdin.read)
  prompt = input_data['prompt'] || ''

  # YAML 규칙 파일 읽기
  unless File.exist?(RULES_FILE)
    # 규칙 파일이 없으면 원본 그대로 반환
    puts JSON.generate(input_data)
    exit 0
  end

  # YAML 파일 파싱
  rules = YAML.load_file(RULES_FILE)

  # 키워드 매칭 및 규칙 활성화
  activated_rules = []
  prompt_lower = prompt.downcase

  rules.each do |rule_name, rule_data|
    keywords = rule_data['keywords'] || []
    trigger_message = rule_data['trigger_message']

    # 키워드 매칭 (대소문자 무시)
    if keywords.any? { |keyword| prompt_lower.include?(keyword.downcase) }
      activated_rules << trigger_message if trigger_message
    end
  end

  # 활성화된 규칙이 있으면 프롬프트 앞에 리마인더 추가
  unless activated_rules.empty?
    reminder = activated_rules.join("\n\n")
    input_data['prompt'] = "#{reminder}\n\n#{prompt}"
  end

  # 수정된 JSON 출력
  puts JSON.generate(input_data)

rescue => e
  # 에러 발생 시 빈 JSON 반환 (Hook이 실패해도 사용자 경험 방해 안함)
  $stderr.puts "Submit hook error: #{e.message}"
  $stderr.puts e.backtrace.first(3).join("\n")

  # stdin은 이미 읽혔으므로 다시 읽지 않고 빈 JSON 반환
  puts JSON.generate({})
  exit 0
end
