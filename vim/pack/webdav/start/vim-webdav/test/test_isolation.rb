#!/usr/bin/env ruby
# WebDAV isolation tests - prove plugin doesn't affect normal Vim operations

require_relative 'test_base'

class TestWebDAVIsolation < TestWebDAVBase
  # Test that normal file operations are completely unaffected
  def test_normal_file_operations_unaffected
    start_vim()

    # Create and edit a normal file
    vim_cmd("edit /tmp/normal-file.txt")
    wait_for_screen_change
    vim_cmd("call append(0, 'NormalFileContent')")
    wait_for_screen_change
    vim_cmd("call append(1, 'Line2')")
    wait_for_screen_change

    # Save normally
    vim_cmd("write")
    wait_for_screen_change

    # Verify buffer is not WebDAV-managed
    vim_cmd("if exists('b:webdav_managed') && b:webdav_managed | echo 'IS_WEBDAV' | else | echo 'NOT_WEBDAV' | endif")
    wait_for_text("NOT_WEBDAV")

    output = capture
    assert_includes output, "NOT_WEBDAV", "Normal file should not be WebDAV-managed"
    refute_match(/Error.*WebDAV/i, output, "Should not see any WebDAV errors")
  end

  # Test mixed buffers (WebDAV + normal files)
  def test_mixed_webdav_and_normal_buffers
    start_vim("WEBDAV_DEFAULT_URL" => "http://localhost:9999")

    # Open normal file first
    vim_cmd("edit /tmp/normal.txt")
    wait_for_screen_change
    vim_cmd("call append(0, 'NormalFile')")
    wait_for_screen_change
    vim_cmd("write")
    wait_for_screen_change

    # Open WebDAV file in new buffer
    vim_cmd("WebDAVGet /test/file1.txt")
    wait_for_text("This is test file content")

    # Verify WebDAV buffer is managed
    vim_cmd("echo exists('b:webdav_managed')")
    wait_for_text("1")

    output = capture
    # exists() returns 1 if variable exists
    assert_match(/1/, output, "WebDAV file should have webdav_managed variable")

    # Switch back to normal buffer
    vim_cmd("bprevious")
    wait_for_screen_change

    # Verify normal buffer is still not managed
    vim_cmd("echo exists('b:webdav_managed')")
    wait_for_text("0")

    output = capture
    # exists() returns 0 if variable doesn't exist
    assert_match(/0/, output, "Normal buffer should not have webdav_managed variable")
  end

  # Test buffer switching preserves independent states
  def test_buffer_switch_preserves_state
    start_vim("WEBDAV_DEFAULT_URL" => "http://localhost:9999")

    # Open WebDAV file
    vim_cmd("WebDAVGet /test/file1.txt")
    wait_for_text("This is test file content")

    # Modify WebDAV buffer
    vim_cmd("call append(0, 'WebDAVModified')")
    wait_for_screen_change

    # Open normal file in new buffer
    vim_cmd("edit /tmp/normal.txt")
    wait_for_screen_change
    vim_cmd("call append(0, 'NormalContent')")
    wait_for_screen_change

    # Check we can switch back and forth without errors
    vim_cmd("bprevious")
    wait_for_screen_change
    vim_cmd("bnext")
    wait_for_screen_change

    output = capture
    # Just verify no errors occurred during buffer switching
    refute_match(/Error|E\d+:/i, output, "Buffer switching should work without errors")
  end

  # Test that WebDAV doesn't interfere with user autocmds
  def test_user_autocmds_still_work
    start_vim("WEBDAV_DEFAULT_URL" => "http://localhost:9999")

    # Set up a user autocmd for normal files
    vim_cmd("let g:user_autocmd_ran = 0")
    wait_for_screen_change
    vim_cmd("autocmd BufWritePre /tmp/*.txt let g:user_autocmd_ran = 1")
    wait_for_screen_change

    # Test with normal file
    vim_cmd("edit /tmp/testfile.txt")
    wait_for_screen_change
    vim_cmd("call append(0, 'TestLine')")
    wait_for_screen_change
    vim_cmd("write")
    wait_for_screen_change

    # Check if user autocmd ran
    vim_cmd("if g:user_autocmd_ran == 1 | echo 'USER_AUTOCMD_OK' | else | echo 'USER_AUTOCMD_FAIL' | endif")
    wait_for_text("USER_AUTOCMD_OK")

    output = capture
    assert_includes output, "USER_AUTOCMD_OK", "User autocmds should still work for normal files"
  end

  # Test WebDAV buffer cleanup on close
  def test_webdav_buffer_cleanup
    start_vim("WEBDAV_DEFAULT_URL" => "http://localhost:9999")

    # Open WebDAV file
    vim_cmd("WebDAVGet /test/file1.txt")
    wait_for_text("This is test file content")

    # Verify it's managed
    vim_cmd("if exists('b:webdav_managed') | echo 'MANAGED' | endif")
    wait_for_text("MANAGED")

    # Close the buffer
    vim_cmd("bdelete!")
    wait_for_screen_change

    # Open a new normal buffer - should be clean
    vim_cmd("enew")
    wait_for_screen_change

    # Verify no WebDAV variables leaked
    vim_cmd("if exists('b:webdav_managed') | echo 'LEAKED' | else | echo 'CLEAN' | endif")
    wait_for_text("CLEAN")

    output = capture
    assert_includes output, "CLEAN", "WebDAV variables should not leak to new buffers"
  end

  # Test that :wq on normal files works without WebDAV interference
  def test_normal_file_wq_works
    start_vim("WEBDAV_DEFAULT_URL" => "http://localhost:9999")

    # Create normal file
    vim_cmd("edit /tmp/wq-test.txt")
    wait_for_screen_change
    vim_cmd("call setline(1, 'Testing wq')")
    wait_for_screen_change

    # Use :wq (write and quit)
    vim_cmd("wq")
    wait_for_screen_change(2)

    # Vim should have exited back to shell
    output = capture
    # Should not see WebDAV-related errors
    refute_match(/Error.*WebDAV|Not a WebDAV buffer/i, output, "Normal file :wq should work without errors")
  end
end
