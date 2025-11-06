#!/usr/bin/env ruby
# WebDAV fzf integration basic tests

require_relative 'test_base'

class TestWebDAVFzf < TestWebDAVBase
  # Test WebDAVFzf command is defined
  def test_webdav_fzf_command_exists
    start_vim("WEBDAV_DEFAULT_URL" => "http://localhost:9999")

    # Check if WebDAVFzf command exists
    vim_cmd("echo exists(':WebDAVFzf')")
    wait_for { capture.match?(/2/) }

    output = capture
    assert_includes output, "2", "WebDAVFzf command should be defined (exists() returns 2 for commands)"
  end

  # Test fzf binary is available in Docker
  def test_fzf_binary_available
    start_vim("WEBDAV_DEFAULT_URL" => "http://localhost:9999")

    # Check if fzf executable exists
    vim_cmd("echo executable('fzf')")
    wait_for { capture.match?(/[01]/) }

    output = capture
    assert_includes output, "1", "fzf binary should be available in Docker environment"
  end

  # Test WebDAVFzf shows error when URL not configured
  def test_webdav_fzf_error_without_url
    start_vim()  # No environment variables

    # Try WebDAVFzf without URL configured
    vim_cmd("WebDAVFzf")
    wait_for_text("Error:", 1)

    output = capture
    assert_match(/Error:|Set WEBDAV_DEFAULT_URL/, output, "Should show error when URL not configured")
  end


end
