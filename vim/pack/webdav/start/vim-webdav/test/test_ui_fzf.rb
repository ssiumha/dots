#!/usr/bin/env ruby
# WebDAV UI fzf integration tests

require_relative 'test_base'

class TestWebDAVUIFzf < TestWebDAVBase
  # Test WebDAVUIFzf command is defined
  def test_webdavuifzf_command_exists
    start_vim("WEBDAV_UI_LOCAL" => "http://localhost:9999")

    # Check if WebDAVUIFzf command exists
    vim_cmd("echo exists(':WebDAVUIFzf')")
    wait_for { capture.match?(/2/) }

    output = capture
    assert_includes output, "2", "WebDAVUIFzf command should be defined (exists() returns 2 for commands)"
  end

  # Test fzf binary is available
  def test_fzf_binary_available
    start_vim("WEBDAV_UI_LOCAL" => "http://localhost:9999")

    # Check if fzf executable exists
    vim_cmd("echo executable('fzf')")
    wait_for { capture.match?(/[01]/) }

    output = capture
    assert_includes output, "1", "fzf binary should be available in Docker environment"
  end

  # Test WebDAVUIFzf shows error when no servers configured
  def test_webdavuifzf_error_without_servers
    start_vim()  # No environment variables

    # Try WebDAVUIFzf without servers configured
    vim_cmd("WebDAVUIFzf")
    wait_for_text("Error:", 1)

    output = capture
    assert_match(/Error:|No WebDAV servers configured/, output, "Should show error when no servers configured")
  end

  # Test WebDAVUIFzf with fallback to WEBDAV_DEFAULT_*
  def test_webdavuifzf_fallback_to_default
    start_vim("WEBDAV_DEFAULT_URL" => "http://localhost:9999")

    send_keys(":WebDAVUIFzf")
    send_enter

    # Press Enter to dismiss fallback message
    send_enter
    wait_for_text("WebDAV:", 2)

    # Should fall back to default and show listing
    output = capture
    assert_includes output, "WebDAV:", "Should show listing using default"
  end

  # Test WebDAVUIFzf launches fzf with server list
  def test_webdavuifzf_launches_fzf
    start_vim(
      "WEBDAV_UI_LOCAL" => "http://localhost:9999",
      "WEBDAV_UI_DEV" => "http://localhost:9999/test"
    )

    # Start WebDAVUIFzf
    vim_cmd("WebDAVUIFzf")
    wait_for_text("WebDAV Server>", 2)

    output = capture
    assert_includes output, "WebDAV Server>", "Should show fzf prompt"
  end

  # Test server selection via fzf
  def test_webdavuifzf_server_selection
    start_vim("WEBDAV_UI_LOCAL" => "http://localhost:9999")

    # Start WebDAVUIFzf
    vim_cmd("WebDAVUIFzf")
    wait_for_text("WebDAV Server>", 2)

    # Should show server in fzf list
    output = capture
    assert_includes output, "local", "Should show server name in fzf list"

    # Press Enter to select first server
    send_enter
    wait_for_text("Connected", 2)

    output = capture
    assert_match(/Connected.*local/i, output, "Should show connection message")

    # Dismiss connection message and wait for listing
    send_enter
    wait_for_text("WebDAV:", 2)

    # Should show directory listing
    output = capture
    assert_includes output, "WebDAV:", "Should show WebDAV listing after selection"
    assert_includes output, "localhost:9999", "Should show connected server URL"
  end

  # Test multiple servers in fzf list
  def test_webdavuifzf_multiple_servers
    start_vim(
      "WEBDAV_UI_LOCAL" => "http://localhost:9999",
      "WEBDAV_UI_DEV" => "http://localhost:9999/test",
      "WEBDAV_UI_PROD" => "http://localhost:9999/prod"
    )

    # Start WebDAVUIFzf
    vim_cmd("WebDAVUIFzf")
    wait_for_text("WebDAV Server>", 2)

    output = capture
    # All servers should be listed
    assert_includes output, "local", "Should show 'local' server"
    assert_includes output, "dev", "Should show 'dev' server"
    assert_includes output, "prod", "Should show 'prod' server"
  end

  # Test server with authentication shows masked password
  def test_webdavuifzf_masked_password
    start_vim("WEBDAV_UI_AUTH" => "http://testuser:testpass@localhost:9999")

    # Start WebDAVUIFzf
    vim_cmd("WebDAVUIFzf")
    wait_for_text("WebDAV Server>", 2)

    output = capture
    assert_includes output, "testuser@...", "Should mask password in fzf display"
    refute_includes output, "testpass", "Should not show actual password"
  end

  # Test selecting second server in list
  def test_webdavuifzf_select_second_server
    start_vim(
      "WEBDAV_UI_LOCAL" => "http://localhost:9999",
      "WEBDAV_UI_DEV" => "http://localhost:9999/test"
    )

    # Start WebDAVUIFzf
    vim_cmd("WebDAVUIFzf")
    wait_for_text("WebDAV Server>", 2)

    # Type 'dev' to filter
    send_keys("dev")
    sleep 0.3

    # Press Enter to select
    send_enter
    wait_for_text("Connected", 2)

    output = capture
    assert_match(/Connected.*dev/i, output, "Should connect to 'dev' server")
  end

  # Test fzf displays server URLs
  def test_webdavuifzf_displays_urls
    start_vim(
      "WEBDAV_UI_LOCAL" => "http://localhost:9999",
      "WEBDAV_UI_TEST" => "http://localhost:9999/test"
    )

    # Start WebDAVUIFzf
    vim_cmd("WebDAVUIFzf")
    wait_for_text("WebDAV Server>", 2)

    output = capture
    assert_includes output, "localhost:9999", "Should show server URL"
  end

  # Test escaping fzf cancels selection
  def test_webdavuifzf_escape_cancels
    start_vim("WEBDAV_UI_LOCAL" => "http://localhost:9999")

    # Start WebDAVUIFzf
    vim_cmd("WebDAVUIFzf")
    wait_for_text("WebDAV Server>", 2)

    # Press Escape to cancel
    docker_exec("tmux send-keys -t test Escape")
    sleep 1

    # Should not show listing (cancelled)
    output = capture
    refute_includes output, "WebDAV:", "Should not show listing after cancellation"
  end
end
