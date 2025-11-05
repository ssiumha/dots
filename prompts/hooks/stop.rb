#!/usr/bin/env ruby
# install:
# {
#   "hooks": {
#     "Stop": [
#       { "hooks": [ { "type": "command", "command": "~/dots/prompts/hooks/stop.rb" } ] }
#     ]
#   }
# }

require 'json'

# =============================================================================
# Stop Hook: 작업 완료 감지 및 문서화 제안
# =============================================================================
#
# 현재 기능:
# - Transcript 분석하여 완료 키워드 감지
# - 완료 시 living-docs 제안 (exit 2 + stderr)
#
# =============================================================================

# TODO: 미래 자동화 아이디어
#
# 1. 자동 Lint 실행
#    - 수정된 파일에 대해 자동 lint 실행
#    - rubocop --auto-correct (Ruby)
#    - eslint --fix (TypeScript/JavaScript)
#    - black (Python)
#
# 2. 자동 Test 실행
#    - 수정된 파일과 관련된 테스트만 실행
#    - npm test -- --only-changed
#    - pytest --lf (last failed)
#
# 3. 작업 완료 검증
#    - 트랜스크립트 분석하여 미완료 작업 감지
#    - 에러 발생 시 decision: "block"으로 Claude 계속 작동
#    - 예: "Error: ENOENT" 같은 미해결 에러 발견 시 차단
#
# 4. Build 검증
#    - 프로젝트 빌드 실행 (optional)
#    - tsc --noEmit (TypeScript 타입 체크)
#    - cargo check (Rust)
#
# =============================================================================

# 완료 키워드 목록
COMPLETION_KEYWORDS = [
  '완료',
  '끝',
  '마무리',
  '완성',
  'done',
  'finished',
  'complete'
].freeze

begin
  # 입력 JSON 파싱
  input_data = JSON.parse($stdin.read)

  # 무한루프 방지: stop_hook_active가 true면 즉시 종료
  if input_data['stop_hook_active']
    puts JSON.generate(input_data)
    exit 0
  end

  # Transcript 파일 경로
  transcript_path = input_data['transcript_path']
  unless transcript_path && File.exist?(File.expand_path(transcript_path))
    puts JSON.generate(input_data)
    exit 0
  end

  # Transcript에서 마지막 3개 메시지 읽기
  transcript_full_path = File.expand_path(transcript_path)
  lines = File.readlines(transcript_full_path).last(3)

  # 완료 키워드 감지
  has_completion = false
  lines.each do |line|
    begin
      entry = JSON.parse(line)
      message_obj = entry['message']
      next unless message_obj

      # Content는 배열 형태로 text 블록들을 포함
      content_blocks = message_obj['content'] || []
      text_contents = content_blocks
        .select { |c| c.is_a?(Hash) && c['type'] == 'text' }
        .map { |c| c['text'] }

      combined_text = text_contents.join(' ').downcase

      if COMPLETION_KEYWORDS.any? { |keyword| combined_text.include?(keyword.downcase) }
        has_completion = true
        break
      end
    rescue JSON::ParserError
      # 파싱 실패한 라인은 무시
      next
    end
  end

  # 완료 감지 시 living-docs 제안
  if has_completion
    $stderr.puts ""
    $stderr.puts "## 📝 Task Completion Detected"
    $stderr.puts ""
    $stderr.puts "Consider documenting this work using the **living-docs** skill:"
    $stderr.puts "- Record architectural decisions (ADR)"
    $stderr.puts "- Update knowledge base"
    $stderr.puts "- Track remaining TODOs"
    $stderr.puts ""
    $stderr.puts "You can trigger it by:"
    $stderr.puts "- Using the `/docs` command"
    $stderr.puts "- Mentioning '문서화' in your message"
    $stderr.puts ""

    # Exit code 2: Claude가 이 메시지를 읽고 처리
    exit 2
  end

  # 정상 완료
  puts JSON.generate(input_data)
  exit 0

rescue => e
  # 에러 발생 시 빈 JSON 반환 (Hook 실패가 사용자 경험을 방해하지 않도록)
  $stderr.puts "Stop hook error: #{e.message}"
  $stderr.puts e.backtrace.first(3).join("\n")

  # stdin은 이미 읽혔으므로 다시 읽지 않고 빈 JSON 반환
  puts JSON.generate({})
  exit 0
end
