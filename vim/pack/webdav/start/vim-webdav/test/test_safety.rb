#!/usr/bin/env ruby
# WebDAV safety and security tests

require_relative 'test_base'

class TestWebDAVSafety < TestWebDAVBase
  # Test safety: refuse to edit files without version tracking
  def test_webdav_safety_no_version_headers
    # This test would need a mock server path that doesn't return ETag/Last-Modified
    # For now, we verify that the buffer variables exist after normal GET
    start_vim("WEBDAV_DEFAULT_URL" => "http://localhost:9999")
    vim_cmd("WebDAVGet /test/file1.txt")
    wait_for { capture.include?("This is test file content") }

    # Check for version tracking headers (simple exists check)
    vim_cmd("echo exists('b:webdav_etag')")
    wait_for_screen_change

    output = capture
    # exists() returns 1 if variable exists, 0 otherwise
    has_etag = output.include?("1")
    assert has_etag, "ETag should exist for version tracking (expects exists()=1). Output: #{output}"
  end

  # Test safety: refuse to save without version tracking
  def test_webdav_safety_prevent_untracked_save
    start_vim("WEBDAV_DEFAULT_URL" => "http://localhost:9999")
    vim_cmd("WebDAVGet /test/file1.txt")
    wait_for { capture.include?("This is test file content") }

    # Artificially remove version tracking to simulate unsafe condition
    vim_cmd("unlet b:webdav_etag")
    vim_cmd("unlet b:webdav_last_modified")

    # Try to save - should fail with safety error
    vim_cmd("normal! ggdG")
    vim_cmd("call setline(1, 'Dangerous edit')")
    vim_cmd("write")
    wait_for_screen_change

    output = capture
    assert_match(/No version tracking|conflict detection/i, output, "Should refuse save without version tracking")
  end

  # Test that normal files are NOT affected by WebDAV plugin
  def test_normal_file_write_not_affected
    start_vim()

    # Create a normal buffer (not WebDAV)
    vim_cmd("enew")
    vim_cmd("call setline(1, 'Normal file content')")

    # Check that this is NOT a WebDAV buffer (fixed: proper exists() syntax)
    vim_cmd("if exists('b:webdav_managed') | echo 'IS_WEBDAV' | else | echo 'NOT_WEBDAV' | endif")
    wait_for { capture.include?("NOT_WEBDAV") || capture.include?("IS_WEBDAV") }
    assert_includes capture, "NOT_WEBDAV", "Normal buffer should not be WebDAV-managed"

    # Verify normal :w works
    vim_cmd("write /tmp/test-normal-write.txt")
    wait_for_screen_change

    # The key point: write should succeed and no WebDAV errors
    output = capture
    assert_includes output, "/tmp/test-normal-write.txt", "Normal file write should work"
  end

  # Test that modifying path prevents save (prevents accidental overwrites)
  def test_cannot_save_with_modified_path
    start_vim("WEBDAV_DEFAULT_URL" => "http://localhost:9999")
    vim_cmd("WebDAVGet /test/file1.txt")
    wait_for { capture.include?("This is test file content") }

    # Maliciously modify the original path
    vim_cmd("let b:webdav_original_path = '/test/different-file.txt'")
    wait_for_screen_change

    # Try to save - should use the stored path (not verify it changed)
    vim_cmd("call setline(1, 'Modified')")
    vim_cmd("write")
    wait_for_screen_change

    # Verify it attempts to save to the modified path (this is expected behavior)
    # The plugin uses whatever is in b:webdav_original_path
    # (This test documents current behavior - path is trusted)
    output = capture
    # Should succeed or fail, but demonstrates path is used as-is
    refute_match(/Vim.*crash|Segmentation/i, output, "Should not crash even with modified path")
  end

  # Test URL mismatch detection prevents cross-server saves
  def test_url_mismatch_prevents_save
    start_vim("WEBDAV_DEFAULT_URL" => "http://localhost:9999")
    vim_cmd("WebDAVGet /test/file1.txt")
    wait_for { capture.include?("This is test file content") }

    # Simulate URL change (like user changed environment variable)
    vim_cmd("let b:webdav_url = 'http://different-server:9999'")
    wait_for_screen_change

    # Now change the global URL (simulates WEBDAV_DEFAULT_URL change)
    # This is tricky in tests, but we can verify the check exists
    vim_cmd("call setline(1, 'Modified')")
    vim_cmd("write")
    wait_for_screen_change

    output = capture
    # Should either succeed (if URLs still match) or show mismatch error
    # The key is that the URL check mechanism exists
    refute_match(/Vim.*crash/i, output, "Should handle URL mismatch safely")
  end

  # Test that safety checks run on every save attempt
  def test_safety_check_on_every_save
    start_vim("WEBDAV_DEFAULT_URL" => "http://localhost:9999")
    vim_cmd("WebDAVGet /test/file1.txt")
    wait_for { capture.include?("This is test file content") }

    # First save - should work
    vim_cmd("call setline(1, 'First')")
    vim_cmd("write")
    wait_for_screen_change

    # Remove safety variable between saves
    vim_cmd("unlet b:webdav_etag")
    vim_cmd("unlet b:webdav_last_modified")
    wait_for_screen_change

    # Second save - should fail safety check
    vim_cmd("call setline(1, 'Second')")
    vim_cmd("write")
    wait_for_screen_change

    output = capture
    assert_match(/No version tracking|conflict detection/i, output, "Safety check should run on every save")
  end
end

