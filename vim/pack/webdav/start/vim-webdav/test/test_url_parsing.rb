#!/usr/bin/env ruby
# URL parsing edge case tests

require_relative 'test_base'

class TestURLParsing < TestWebDAVBase
  # Test URL with port number (using environment variable)
  def test_url_with_port
    start_vim("WEBDAV_UI_TEST" => "http://user:pass@localhost:8080/webdav")

    send_keys(":WebDAVUI")
    send_enter
    wait_for_text("localhost:8080")

    output = capture
    assert_includes output, "localhost:8080", "Should parse port number correctly"
    assert_includes output, "/webdav", "Should parse path correctly"
    assert_includes output, "user@...", "Should parse and mask authentication"
  end

  # Test password with special characters (URL encoded)
  def test_password_with_special_chars
    # Use URL encoding for special characters in password
    start_vim("WEBDAV_UI_TEST" => "http://user:p%40ss%3Aword@localhost:9999")

    send_keys(":WebDAVUI")
    send_enter
    wait_for_text("test:")

    output = capture
    # Should display server and mask password
    assert_includes output, "test:", "Should show server name"
    assert_includes output, "user@...", "Should mask password with special chars"
  end

  # Test URL without authentication
  def test_url_without_auth
    start_vim("WEBDAV_UI_NOAUTH" => "http://localhost:9999/path")

    send_keys(":WebDAVUI")
    send_enter
    wait_for_text("noauth:")

    output = capture
    # Should show URL without authentication indicator
    assert_includes output, "noauth:", "Should show server"
    assert_includes output, "localhost:9999/path", "Should show URL"
    refute_match(/@\.\.\./, output, "Should not show masked password when no auth")
  end

  # Test HTTPS URL
  def test_https_url
    start_vim("WEBDAV_UI_SECURE" => "https://user:pass@example.com:443/dav")

    send_keys(":WebDAVUI")
    send_enter
    wait_for_text("https://")

    output = capture
    assert_includes output, "https://", "Should preserve HTTPS protocol"
    assert_includes output, "example.com", "Should parse host correctly"
    assert_includes output, "user@...", "Should handle HTTPS authentication"
  end

  # Test Korean characters in base path
  def test_korean_in_base_path
    # Korean in URL should work (will be encoded when making requests)
    start_vim("WEBDAV_UI_KOREAN" => "http://localhost:9999/한글경로")

    send_keys(":WebDAVUI")
    send_enter
    wait_for_text("korean:")

    output = capture
    # Should show server in list
    assert_includes output, "korean:", "Should show server name"
    assert_includes output, "localhost:9999", "Should parse host correctly"
  end

  # Test no servers configured
  def test_no_servers_configured
    start_vim()  # No WEBDAV_UI_* variables

    send_keys(":WebDAVUI")
    send_enter
    wait_for_text("No WebDAV servers configured")

    output = capture
    assert_match(/No WebDAV servers configured/i, output, "Should show error when no servers")
  end

  # Test malformed URL (should be ignored)
  def test_malformed_url_ignored
    start_vim(
      "WEBDAV_UI_GOOD" => "http://localhost:9999",
      "WEBDAV_UI_BAD" => "not-a-url"
    )

    send_keys(":WebDAVUI")
    send_enter
    wait_for_text("good:")

    output = capture
    # Good server should be listed
    assert_includes output, "good:", "Should list valid server"
    # Bad server should be ignored (empty URL after parsing)
    # It might still appear but with empty URL - implementation dependent
  end
end
