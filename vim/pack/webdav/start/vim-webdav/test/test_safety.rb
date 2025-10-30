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
    sleep 0.2

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
    sleep 0.5

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
    sleep 0.3

    # The key point: write should succeed and no WebDAV errors
    output = capture
    assert_includes output, "/tmp/test-normal-write.txt", "Normal file write should work"
  end
end
