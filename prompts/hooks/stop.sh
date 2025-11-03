#!/usr/bin/env ruby
# install:
# {
#   "hooks": {
#     "Stop": [
#       { "hooks": [ { "type": "command", "command": "~/dots/prompts/hooks/stop.sh" } ] }
#     ]
#   }
# }

require 'json'
require 'yaml'

# 스크립트 디렉토리 경로
SCRIPT_DIR = File.expand_path('~/dots/prompts/hooks')
RULES_FILE = File.join(SCRIPT_DIR, 'stop-rules.yaml')

def parse_transcript(transcript_path)
  """트랜스크립트 JSONL 파일에서 수정된 파일 목록 추출"""
  modified_files = []

  return modified_files unless File.exist?(transcript_path)

  File.readlines(transcript_path).each do |line|
    begin
      event = JSON.parse(line.strip)

      # tool_result 타입 이벤트에서 Edit/Write 찾기
      if event['type'] == 'tool_result'
        tool_name = event.dig('message', 'content')&.find { |c| c['type'] == 'tool_use' }&.dig('name')

        # Edit 또는 Write tool 사용 시 파일 경로 추출
        if ['Edit', 'Write'].include?(tool_name)
          event.dig('message', 'content')&.each do |content|
            if content['type'] == 'tool_use'
              file_path = content.dig('input', 'file_path')
              modified_files << file_path if file_path
            end
          end
        end
      end
    rescue JSON::ParserError
      next
    end
  end

  modified_files.uniq
end

def load_rules
  """YAML 파일에서 규칙 로드, 없으면 nil 반환"""
  return nil unless File.exist?(RULES_FILE)
  YAML.load_file(RULES_FILE)
end

def should_exclude?(file_path, exclude_patterns)
  """파일이 제외 패턴에 해당하는지 체크"""
  exclude_patterns.any? do |pattern|
    # 간단한 glob 패턴 매칭
    regex = pattern.gsub('**/', '.*').gsub('*', '[^/]*').gsub('.', '\.')
    file_path.match?(/#{regex}/)
  end
end

def check_test_coverage(modified_files, rule)
  """테스트 커버리지 체크"""
  return [] unless rule['enabled']

  reminders = []
  patterns = rule['patterns'] || []
  exclude_patterns = rule['exclude_patterns'] || []

  source_files = modified_files.select do |file|
    # 패턴에 매칭되고 제외 패턴에 안 걸리는 파일
    matches_pattern = patterns.any? { |p| file.end_with?(p.gsub('**/*', '')) }
    matches_pattern && !should_exclude?(file, exclude_patterns)
  end

  if source_files.any?
    source_files.each do |file|
      reminders << "- #{file}: #{rule['message']}"
    end
  end

  reminders
end

def check_living_docs(modified_files, rule)
  """living-docs 업데이트 필요 여부 체크"""
  return [] unless rule['enabled']

  # 의미있는 변경이 있는지 간단히 체크
  # (실제로는 더 정교한 로직 필요)
  if modified_files.length >= 2
    ["- Consider updating living-docs: #{rule['message']}"]
  else
    []
  end
end

begin
  # 입력 JSON 파싱
  input_data = JSON.parse($stdin.read)

  # 무한루프 방지: stop_hook_active가 true면 즉시 종료
  if input_data['stop_hook_active']
    puts JSON.generate(input_data)
    exit 0
  end

  transcript_path = input_data['transcript_path']

  # 트랜스크립트 파일이 없으면 종료
  unless transcript_path && File.exist?(transcript_path)
    puts JSON.generate(input_data)
    exit 0
  end

  # 수정된 파일 목록 추출
  modified_files = parse_transcript(transcript_path)

  # 파일 수정이 없으면 종료
  if modified_files.empty?
    puts JSON.generate(input_data)
    exit 0
  end

  # 규칙 로드
  rules = load_rules

  # 규칙 파일이 없으면 종료
  unless rules
    puts JSON.generate(input_data)
    exit 0
  end

  checks = rules['checks'] || {}

  # 각 체크 실행
  all_reminders = []

  if checks['test_coverage']
    all_reminders.concat(check_test_coverage(modified_files, checks['test_coverage']))
  end

  if checks['living_docs']
    all_reminders.concat(check_living_docs(modified_files, checks['living_docs']))
  end

  # 리마인더가 있으면 출력
  unless all_reminders.empty?
    puts "\n❓ Quality Check\n\n"
    all_reminders.each { |reminder| puts reminder }
    puts "\n"
  end

  # 원본 JSON 그대로 반환 (비블로킹)
  puts JSON.generate(input_data)
  exit 0

rescue => e
  # 에러 발생 시 원본 그대로 반환
  $stderr.puts "Stop hook error: #{e.message}"
  $stderr.puts e.backtrace.first(3).join("\n")

  begin
    input_data = JSON.parse($stdin.read)
  rescue
    input_data = {}
  end

  puts JSON.generate(input_data)
  exit 0
end
