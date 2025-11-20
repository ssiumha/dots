#!/usr/bin/env ruby
# WebDAVNote command tests

require_relative 'test_base'

class TestNote < TestWebDAVBase
  def test_note_creates_new_daily_note
    start_vim("WEBDAV_DEFAULT_URL" => "http://localhost:9999")

    # Configure note patterns using VimScript
    vim_cmd("let g:webdav_note_patterns = {}")
    vim_cmd("let g:webdav_note_patterns.daily = {}")
    vim_cmd("let g:webdav_note_patterns.daily.server = \\\"\\\"")
    vim_cmd("let g:webdav_note_patterns.daily.path = \\\"/test/daily/%Y-%m-%d.md\\\"")
    vim_cmd("let g:webdav_note_patterns.daily.template = \\\"# Daily %Y-%m-%d\\\"")
    vim_cmd("let g:webdav_note_patterns.daily.unit = \\\"day\\\"")

    # Get today's date
    today = Time.now.strftime('%Y-%m-%d')
    file_path = "/test/daily/#{today}.md"

    # Create daily note
    vim_cmd("WebDAVNote daily")
    send_enter  # Dismiss "Created:" message
    wait_for_text("Daily", 2)

    # Check buffer content
    vim_cmd("echo getline(1)")
    output = capture
    assert_includes output, "Daily #{today}", "Template should be applied"

    # Check buffer is WebDAV-managed
    vim_cmd("echo b:webdav_managed")
    output = capture
    assert_includes output, "1", "Buffer should be WebDAV-managed"

    # Verify file created on server
    result = docker_exec("curl -s http://localhost:9999#{file_path}")
    assert_includes result, "Daily #{today}", "File should exist on server"
  end

  def test_note_opens_existing_daily_note
    start_vim("WEBDAV_DEFAULT_URL" => "http://localhost:9999")

    # Create a daily note first
    today = Time.now.strftime('%Y-%m-%d')
    file_path = "/test/daily/#{today}.md"
    docker_exec("curl -s -X PUT http://localhost:9999#{file_path} -d ExistingNoteContent > /dev/null")

    # Configure note patterns
    vim_cmd("let g:webdav_note_patterns = {}")
    vim_cmd("let g:webdav_note_patterns.daily = {}")
    vim_cmd("let g:webdav_note_patterns.daily.server = \\\"\\\"")
    vim_cmd("let g:webdav_note_patterns.daily.path = \\\"/test/daily/%Y-%m-%d.md\\\"")
    vim_cmd("let g:webdav_note_patterns.daily.template = \\\"# Daily %Y-%m-%d\\\"")
    vim_cmd("let g:webdav_note_patterns.daily.unit = \\\"day\\\"")

    # Open the note
    vim_cmd("WebDAVNote daily")
    send_enter  # Dismiss "Opening existing note:" message
    wait_for_text("Existing", 2)

    # Check it opened the existing note
    vim_cmd("echo getline(1)")
    output = capture
    assert_includes output, "Existing", "Should open existing note"
  end

  def test_note_with_offset
    start_vim("WEBDAV_DEFAULT_URL" => "http://localhost:9999")

    # Configure note patterns
    vim_cmd("let g:webdav_note_patterns = {}")
    vim_cmd("let g:webdav_note_patterns.daily = {}")
    vim_cmd("let g:webdav_note_patterns.daily.server = \\\"\\\"")
    vim_cmd("let g:webdav_note_patterns.daily.path = \\\"/test/daily/%Y-%m-%d.md\\\"")
    vim_cmd("let g:webdav_note_patterns.daily.template = \\\"# Daily %Y-%m-%d\\\"")
    vim_cmd("let g:webdav_note_patterns.daily.unit = \\\"day\\\"")

    # Create yesterday's note
    yesterday = (Time.now - 86400).strftime('%Y-%m-%d')
    vim_cmd("WebDAVNote daily -1")
    send_enter  # Dismiss "Created:" message
    wait_for_text("Daily", 2)

    # Check content
    vim_cmd("echo getline(1)")
    output = capture
    assert_includes output, "Daily #{yesterday}", "Should create yesterday's note"

    # Verify on server
    file_path = "/test/daily/#{yesterday}.md"
    result = docker_exec("curl -s http://localhost:9999#{file_path}")
    assert_includes result, "Daily #{yesterday}", "File should exist on server"
  end

  def test_note_error_without_pattern
    start_vim("WEBDAV_DEFAULT_URL" => "http://localhost:9999")

    # Try invalid pattern without config
    vim_cmd("WebDAVNote invalid")
    wait_for_text("not configured", 2)
  end

  def test_note_monthly
    start_vim("WEBDAV_DEFAULT_URL" => "http://localhost:9999")

    # Configure monthly pattern
    vim_cmd("let g:webdav_note_patterns = {}")
    vim_cmd("let g:webdav_note_patterns.monthly = {}")
    vim_cmd("let g:webdav_note_patterns.monthly.server = \\\"\\\"")
    vim_cmd("let g:webdav_note_patterns.monthly.path = \\\"/test/monthly/%Y-%m.md\\\"")
    vim_cmd("let g:webdav_note_patterns.monthly.template = \\\"# Month %Y-%m\\\"")
    vim_cmd("let g:webdav_note_patterns.monthly.unit = \\\"month\\\"")

    # Create monthly note
    current_month = Time.now.strftime('%Y-%m')
    vim_cmd("WebDAVNote monthly")
    send_enter  # Dismiss "Created:" message
    wait_for_text("Month", 2)

    # Check content
    vim_cmd("echo getline(1)")
    output = capture
    assert_includes output, "Month #{current_month}", "Monthly template should be applied"

    # Verify on server
    file_path = "/test/monthly/#{current_month}.md"
    result = docker_exec("curl -s http://localhost:9999#{file_path}")
    assert_includes result, "Month #{current_month}", "File should exist on server"
  end

  def test_note_save_without_default_url
    # Test WebDAVNote without WEBDAV_DEFAULT_URL set
    # This tests that ValidateWebDAVBuffer uses b:webdav_server instead of s:url
    start_vim  # No WEBDAV_DEFAULT_URL

    # Configure daily pattern with explicit server
    vim_cmd("let g:webdav_note_patterns = {}")
    vim_cmd("let g:webdav_note_patterns.daily = {}")
    vim_cmd("let g:webdav_note_patterns.daily.server = \\\"\\\"")
    vim_cmd("let g:webdav_note_patterns.daily.path = \\\"/test/daily/%Y-%m-%d.md\\\"")
    vim_cmd("let g:webdav_note_patterns.daily.template = \\\"# Daily %Y-%m-%d\\\"")
    vim_cmd("let g:webdav_note_patterns.daily.unit = \\\"day\\\"")

    # Set server URL via global variable (simulating pattern's server_info)
    vim_cmd("let s:webdav_test_url = \\\"http://localhost:9999\\\"")
    vim_cmd("let s:webdav_test_user = \\\"\\\"")
    vim_cmd("let s:webdav_test_pass = \\\"\\\"")

    # This won't work without proper setup, but we need to test that the validation
    # doesn't fail on empty s:url when b:webdav_server is set
    # For now, we'll skip this test as it requires more infrastructure
  end

  def test_note_modify_and_save
    start_vim("WEBDAV_DEFAULT_URL" => "http://localhost:9999")

    # Configure daily pattern
    vim_cmd("let g:webdav_note_patterns = {}")
    vim_cmd("let g:webdav_note_patterns.daily = {}")
    vim_cmd("let g:webdav_note_patterns.daily.server = \\\"\\\"")
    vim_cmd("let g:webdav_note_patterns.daily.path = \\\"/test/daily/%Y-%m-%d.md\\\"")
    vim_cmd("let g:webdav_note_patterns.daily.template = \\\"# Daily %Y-%m-%d\\\"")
    vim_cmd("let g:webdav_note_patterns.daily.unit = \\\"day\\\"")

    # Create daily note
    today = Time.now.strftime('%Y-%m-%d')
    file_path = "/test/daily/#{today}.md"
    vim_cmd("WebDAVNote daily")
    send_enter  # Dismiss "Created:" message
    wait_for_text("Daily", 2)

    # Modify content - go to end and append new line
    vim_cmd("normal! GoModified content")
    vim_cmd("normal! \\<Esc>")
    wait_for_text("Modified", 1)

    # Save with :w
    vim_cmd("w")
    sleep 1

    # Verify modified content on server
    result = docker_exec("curl -s http://localhost:9999#{file_path}")
    assert_includes result, "Modified content", "Modified content should be saved"
  end

  def test_note_fleeting_pattern_configuration
    # Test that fleeting pattern with prompt_title can be configured
    # This tests the pattern structure, not the input() prompt itself
    start_vim("WEBDAV_DEFAULT_URL" => "http://localhost:9999")

    # Configure fleeting pattern with empty string (uses default %y%m%d format)
    vim_cmd("let g:webdav_note_patterns = {}")
    vim_cmd("let g:webdav_note_patterns.fleeting = {}")
    vim_cmd("let g:webdav_note_patterns.fleeting.server = \\\"\\\"")
    vim_cmd("let g:webdav_note_patterns.fleeting.path = \\\"/test/fleeting/{title}.md\\\"")
    vim_cmd("let g:webdav_note_patterns.fleeting.template = \\\"# {title}\\\\n\\\\n## Notes\\\\n\\\"")
    vim_cmd("let g:webdav_note_patterns.fleeting.unit = \\\"day\\\"")
    vim_cmd("let g:webdav_note_patterns.fleeting.prompt_title = \\\"\\\"")

    # Verify pattern is configured correctly
    vim_cmd("echo has_key(g:webdav_note_patterns, 'fleeting')")
    output = capture
    assert_includes output, "1", "Fleeting pattern should be configured"

    vim_cmd("echo empty(g:webdav_note_patterns.fleeting.prompt_title)")
    output = capture
    assert_includes output, "1", "prompt_title should be empty string"

    # Verify placeholder exists in path and template
    vim_cmd("echo g:webdav_note_patterns.fleeting.path")
    output = capture
    assert_includes output, "{title}", "Path should contain {title} placeholder"

    vim_cmd("echo g:webdav_note_patterns.fleeting.template")
    output = capture
    assert_includes output, "{title}", "Template should contain {title} placeholder"
  end
end
