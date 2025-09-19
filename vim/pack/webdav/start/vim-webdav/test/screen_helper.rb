# Screen assertion helpers for testing vim output
module ScreenHelper
  # Check if screen contains expected lines (partial match)
  # Lines must appear in order but don't need to be consecutive
  def assert_screen_includes(expected, message = nil)
    output = capture
    expected_lines = expected.strip.split("\n")
    actual_lines = output.split("\n")

    # Find each expected line in order
    last_index = -1
    expected_lines.each do |expected_line|
      found_index = actual_lines.index { |line|
        actual_lines.index(line) > last_index && line.strip == expected_line.strip
      }

      if found_index.nil?
        flunk "#{message || 'Screen content mismatch'}\nExpected to find: #{expected_line}\nActual screen:\n#{output}"
      end
      last_index = found_index
    end

    pass # All lines found in order
  end

  # Check if screen contains text block exactly as shown
  # The block must appear as a continuous section
  def assert_screen_block(expected, message = nil)
    output = capture
    expected_lines = expected.strip.split("\n")
    actual_lines = output.split("\n")

    # Find where the block starts
    start_index = actual_lines.index(expected_lines.first)

    if start_index.nil?
      flunk "#{message || 'Screen block not found'}\nExpected first line: #{expected_lines.first}\nActual screen:\n#{output}"
    end

    # Check if all lines match from that point
    expected_lines.each_with_index do |expected_line, i|
      actual_line = actual_lines[start_index + i]

      if actual_line != expected_line
        flunk "#{message || 'Screen block mismatch'}\nLine #{i+1} differs:\nExpected: #{expected_line}\nActual: #{actual_line || '(no line)'}"
      end
    end

    pass
  end

  # Check if screen matches pattern with wildcards
  # Use ... to match any content between lines
  def assert_screen_pattern(pattern, message = nil)
    output = capture
    pattern_lines = pattern.strip.split("\n")
    actual_lines = output.split("\n")

    actual_index = 0
    pattern_lines.each do |pattern_line|
      if pattern_line.strip == "..."
        # Skip to next pattern line
        next
      end

      # Find matching line
      found = false
      while actual_index < actual_lines.size
        if actual_lines[actual_index].strip == pattern_line.strip
          found = true
          actual_index += 1
          break
        end
        actual_index += 1
      end

      unless found
        flunk "#{message || 'Pattern not matched'}\nCouldn't find: #{pattern_line}\nAfter line #{actual_index}"
      end
    end

    pass
  end

  # Get visible content without vim chrome (status lines, ~, etc)
  def content_lines
    output = capture
    output.split("\n").take_while { |line| !line.start_with?("~") }
  end

  # Assert the header line matches
  def assert_header(expected_header)
    lines = content_lines
    assert_equal expected_header, lines.first, "Header mismatch"
  end

  # Assert file list contains expected files
  def assert_files_listed(*files)
    lines = content_lines[1..-1] # Skip header
    files.each do |file|
      assert lines.include?(file), "Expected to find '#{file}' in file list"
    end
  end
end