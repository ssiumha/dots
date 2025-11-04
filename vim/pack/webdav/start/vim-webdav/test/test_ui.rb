#!/usr/bin/env ruby
# WebDAV server selection UI tests

require_relative 'test_base'

class TestWebDAVUI < TestWebDAVBase
  # Test WebDAVUI shows multiple servers
  def test_webdav_ui_shows_servers
    start_vim(
      "WEBDAV_UI_LOCAL" => "http://localhost:9999",
      "WEBDAV_UI_DEV" => "http://localhost:9999/test"
    )

    # Open WebDAVUI to see server list
    send_keys(":WebDAVUI")
    send_enter
    wait_for_text("Select WebDAV Server")

    # Should show server selection menu
    output = capture
    assert_includes output, "Select WebDAV Server", "Should show selection prompt"
    assert_includes output, "local", "Should list local server"
    assert_includes output, "dev", "Should list dev server"
  end

  # Test WebDAVUI command with single server
  def test_webdav_ui_single_server
    start_vim("WEBDAV_UI_LOCAL" => "http://localhost:9999")

    # Simulate selecting option 1
    send_keys(":WebDAVUI")
    send_enter
    wait_for_text("Select WebDAV Server")

    # Enter selection "1"
    send_keys("1")
    send_enter
    wait_for_text("Connected")

    # Press Enter again to dismiss "Connected" message
    send_enter
    wait_for_text("WebDAV:")

    # Should see WebDAV listing
    output = capture
    assert_includes output, "WebDAV:", "Should show WebDAV listing after selection"
  end

  # Test WebDAVUI with multiple servers
  def test_webdav_ui_multiple_servers
    start_vim(
      "WEBDAV_UI_LOCAL" => "http://localhost:9999",
      "WEBDAV_UI_TEST" => "http://localhost:9999/test"
    )

    send_keys(":WebDAVUI")
    send_enter
    wait_for_text("Select WebDAV Server")

    # Should show selection menu with both servers
    output = capture
    assert_includes output, "local", "Should show 'local' in server list"
    assert_includes output, "test", "Should show 'test' in server list"

    # Select first server (1)
    send_keys("1")
    send_enter
    wait_for_text("Connected")

    # Should connect to local server
    output = capture
    assert_includes output, "localhost:9999", "Should connect to selected server"
  end

  # Test URL with authentication in environment variable
  def test_webdav_ui_with_auth_in_url
    start_vim("WEBDAV_UI_AUTH" => "http://testuser:testpass@localhost:9999")

    send_keys(":WebDAVUI")
    send_enter
    wait_for_text("Select WebDAV Server")

    # Should show masked password in selection
    output = capture
    assert_includes output, "testuser@...", "Should mask password in display"

    # Select server
    send_keys("1")
    send_enter
    wait_for_text("Connected")

    # Press Enter to dismiss connection message
    send_enter
    wait_for_text("WebDAV:")

    # Should successfully connect (authentication was parsed)
    output = capture
    assert_includes output, "WebDAV:", "Should connect with parsed credentials"
  end

  # Test same server with different paths (treated as separate servers)
  def test_webdav_ui_same_server_different_paths
    start_vim(
      "WEBDAV_UI_PROJECT_A" => "http://localhost:9999/test",
      "WEBDAV_UI_PROJECT_B" => "http://localhost:9999/test/deep"
    )

    send_keys(":WebDAVUI")
    send_enter
    wait_for_text("Select WebDAV Server")

    # Should show both as separate entries
    output = capture
    assert_includes output, "project_a", "Should list project_a"
    assert_includes output, "project_b", "Should list project_b"
    assert_includes output, "/test", "Should show path for project_a"
    assert_includes output, "/test/deep", "Should show path for project_b"

    # Select second server (project_b with deeper path)
    send_keys("2")
    send_enter
    wait_for_text("Connected")

    # Should connect to the deep path
    output = capture
    assert_includes output, "/test/deep", "Should connect to deeper path"
  end

  # Test fallback to WEBDAV_DEFAULT_* when no WEBDAV_UI_* found
  def test_webdav_ui_fallback_to_default
    start_vim("WEBDAV_DEFAULT_URL" => "http://localhost:9999")

    send_keys(":WebDAVUI")
    send_enter
    wait_for_screen_change

    # Press Enter to dismiss fallback message
    send_enter
    wait_for_text("WebDAV:")

    # Should fall back to default and show listing
    output = capture
    assert_includes output, "WebDAV:", "Should still show listing using default"
  end

  # Test error when no servers configured
  def test_webdav_ui_no_servers_configured
    start_vim()  # No WebDAV environment variables

    send_keys(":WebDAVUI")
    send_enter
    wait_for_text("No WebDAV servers configured")

    output = capture
    assert_includes output, "No WebDAV servers configured", "Should show error message"
  end

  # Test cancelling server selection
  def test_webdav_ui_cancel_selection
    start_vim("WEBDAV_UI_LOCAL" => "http://localhost:9999")

    send_keys(":WebDAVUI")
    send_enter
    wait_for_text("Select WebDAV Server")

    # Press 0 or invalid number to cancel
    send_keys("0")
    send_enter
    wait_for_screen_change

    output = capture
    assert_match(/Cancelled|invalid/i, output, "Should show cancellation message")
  end

  # Test server name case conversion (uppercase to lowercase)
  def test_webdav_ui_name_case_conversion
    start_vim(
      "WEBDAV_UI_PRODUCTION" => "http://localhost:9999",
      "WEBDAV_UI_STAGING" => "http://localhost:9999/test"
    )

    # Open WebDAVUI - names should be converted to lowercase in display
    send_keys(":WebDAVUI")
    send_enter
    wait_for_text("Select WebDAV Server")

    output = capture
    # Server names should appear as lowercase
    assert_includes output, "production", "PRODUCTION should be converted to lowercase"
    assert_includes output, "staging", "STAGING should be converted to lowercase"
  end

  # Test selecting and connecting to server
  def test_server_selection_and_connection
    start_vim("WEBDAV_UI_LOCAL" => "http://localhost:9999")

    send_keys(":WebDAVUI")
    send_enter
    wait_for_text("Select WebDAV Server")

    # Should show server list
    output = capture
    assert_includes output, "local", "Should show server in list"

    # Select server (1)
    send_keys("1")
    send_enter
    wait_for_text("Connected")

    # Press Enter to dismiss connection message
    send_enter
    wait_for_text("WebDAV:")

    # Should connect and show root listing
    output = capture
    assert_includes output, "WebDAV:", "Should show WebDAV listing after connection"
    assert_includes output, "localhost:9999", "Should show connected server URL"
  end

  # Test direct server selection with argument
  def test_direct_server_selection
    start_vim(
      "WEBDAV_UI_LOCAL" => "http://localhost:9999",
      "WEBDAV_UI_DEV" => "http://localhost:9999/test"
    )

    # Directly select 'dev' server
    send_keys(":WebDAVUI dev")
    send_enter
    wait_for_text("Connected")

    # Press Enter to dismiss connection message
    send_enter
    wait_for_text("WebDAV:")

    # Should connect directly without menu
    output = capture
    assert_includes output, "WebDAV:", "Should show WebDAV listing"
    assert_includes output, "/test/", "Should connect to dev server path"
  end

  # Test direct selection with wrong server name
  def test_direct_selection_wrong_server
    start_vim(
      "WEBDAV_UI_LOCAL" => "http://localhost:9999",
      "WEBDAV_UI_DEV" => "http://localhost:9999/test"
    )

    # Try to select non-existent server
    send_keys(":WebDAVUI wrong")
    send_enter
    wait_for_text("not found")

    output = capture
    assert_match(/Server 'wrong' not found/i, output, "Should show error for wrong server")
    assert_includes output, "Available servers:", "Should show available servers"
    assert_includes output, "dev", "Should list dev server"
    assert_includes output, "local", "Should list local server"
  end

  # Test direct selection is case-sensitive (lowercase)
  def test_direct_selection_case_sensitive
    start_vim("WEBDAV_UI_PRODUCTION" => "http://localhost:9999")

    # Server name is stored as lowercase 'production'
    send_keys(":WebDAVUI production")
    send_enter
    wait_for_text("Connected")

    # Press Enter to dismiss connection message
    send_enter
    wait_for_text("WebDAV:")

    output = capture
    assert_includes output, "WebDAV:", "Should show listing"
  end

  # Test tab completion support (just verify command works)
  def test_direct_selection_with_spaces
    start_vim("WEBDAV_UI_LOCAL" => "http://localhost:9999")

    # Extra spaces should be trimmed
    send_keys(":WebDAVUI  local  ")
    send_enter
    wait_for_text("Connected")

    # Press Enter to dismiss connection message
    send_enter
    wait_for_text("WebDAV:")

    output = capture
    assert_includes output, "WebDAV:", "Should connect successfully"
  end
end
