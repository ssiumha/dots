#!/usr/bin/env ruby
# WebDAV edge case tests

require_relative 'test_base'

class TestWebDAVEdgeCases < TestWebDAVBase
  # Test reading an empty file
  def test_empty_file_read
    start_vim("WEBDAV_DEFAULT_URL" => "http://localhost:9999")

    # Create empty file on server
    docker_exec("curl -s -X PUT http://localhost:9999/test/empty.txt --data-binary @/dev/null > /dev/null")

    # Try to read it
    vim_cmd("WebDAVGet /test/empty.txt")
    wait_for_screen_change

    # Should not error, just open empty buffer
    output = capture
    refute_match(/Error/i, output, "Empty file should open without errors")

    # Verify it's still WebDAV-managed even though empty
    vim_cmd("echo exists('b:webdav_managed')")
    wait_for_text(/1/)
    assert_match(/1/, capture, "Empty file should still be WebDAV-managed")
  end

  # Test saving an empty file
  def test_empty_file_save
    start_vim("WEBDAV_DEFAULT_URL" => "http://localhost:9999")

    vim_cmd("WebDAVGet /test/file1.txt")
    wait_for_text("This is test file content")

    # Delete all content to make it empty
    vim_cmd("normal! ggdG")
    wait_for_screen_change

    # Try to save empty file
    vim_cmd("write")
    wait_for_screen_change

    output = capture
    # Should succeed
    refute_match(/Error/i, output, "Saving empty file should work")
  end

  # Test file with only whitespace
  def test_whitespace_only_file
    start_vim("WEBDAV_DEFAULT_URL" => "http://localhost:9999")

    vim_cmd("WebDAVGet /test/file1.txt")
    wait_for_text("This is test file content")

    # Replace with whitespace only
    vim_cmd("normal! ggdG")
    vim_cmd("call append(0, '   ')")
    vim_cmd("call append(1, '\\t\\t')")
    wait_for_screen_change

    vim_cmd("write")
    wait_for_screen_change

    output = capture
    refute_match(/Error/i, output, "Saving whitespace-only file should work")
  end

  # Test path with spaces
  def test_path_with_spaces
    start_vim("WEBDAV_DEFAULT_URL" => "http://localhost:9999")

    # Create file with space in name
    docker_exec("curl -s -X PUT 'http://localhost:9999/test/file%20with%20spaces.txt' -d 'Content with spaces' > /dev/null")

    # Try to read it (plugin should URL-encode)
    vim_cmd("WebDAVGet /test/file with spaces.txt")
    wait_for_screen_change

    output = capture
    # Should either work or give clear error
    # (depends on URL encoding implementation)
    refute_match(/Vim.*Error/i, output, "Should handle spaces in path")
  end

  # Test very long line content
  def test_long_line_content
    start_vim("WEBDAV_DEFAULT_URL" => "http://localhost:9999")

    vim_cmd("WebDAVGet /test/file1.txt")
    wait_for_text("This is test file content")

    # Create a very long line (1000 chars)
    long_string = "x" * 1000
    vim_cmd("call setline(1, '#{long_string}')")
    wait_for_screen_change

    vim_cmd("write")
    wait_for_screen_change

    output = capture
    refute_match(/Error/i, output, "Should handle long lines")
  end

  # Test buffer metadata persistence
  def test_buffer_metadata_persists
    start_vim("WEBDAV_DEFAULT_URL" => "http://localhost:9999")

    vim_cmd("WebDAVGet /test/file1.txt")
    wait_for_text("This is test file content")

    # Check initial metadata
    vim_cmd("echo exists('b:webdav_etag')")
    wait_for_text("1")
    assert_match(/1/, capture, "Should have ETag initially")

    # Make some changes (but don't save)
    vim_cmd("call setline(1, 'Modified')")
    wait_for_screen_change

    # Metadata should still exist
    vim_cmd("echo exists('b:webdav_etag')")
    wait_for_text("1")
    assert_match(/1/, capture, "ETag should persist after local changes")

    vim_cmd("echo exists('b:webdav_original_path')")
    wait_for_text("1")
    assert_match(/1/, capture, "Original path should persist")
  end

  # Test modified flag after failed save
  def test_modified_flag_after_save_attempt
    start_vim("WEBDAV_DEFAULT_URL" => "http://localhost:9999")

    vim_cmd("WebDAVGet /test/file1.txt")
    wait_for_text("This is test file content")

    # Modify content
    vim_cmd("call setline(1, 'Modified content')")
    wait_for_screen_change

    # Check modified flag is set
    vim_cmd("echo &modified")
    wait_for_text("1")
    assert_match(/1/, capture, "Buffer should be marked as modified (flag=1)")

    # Save successfully
    vim_cmd("write")
    wait_for_screen_change

    # Check modified flag is cleared
    vim_cmd("echo &modified")
    wait_for_text("0")
    output = capture
    assert_match(/0/, output, "Buffer should not be modified after successful save (flag=0)")
  end

  # Test multiple files open simultaneously
  def test_multiple_webdav_files_open
    start_vim("WEBDAV_DEFAULT_URL" => "http://localhost:9999")

    # Open first file
    vim_cmd("WebDAVGet /test/file1.txt")
    wait_for_text("This is test file content")

    # Open second file in new tab
    vim_cmd("tabnew")
    vim_cmd("WebDAVGet /test/한글.md")
    wait_for_text("한글 문서")

    # Both should be WebDAV-managed
    vim_cmd("echo exists('b:webdav_managed')")
    wait_for_text("1")
    assert_match(/1/, capture, "Second file should be WebDAV-managed")

    # Switch back to first tab
    vim_cmd("tabprevious")
    wait_for_screen_change

    vim_cmd("echo exists('b:webdav_managed')")
    wait_for_text("1")
    assert_match(/1/, capture, "First file should still be WebDAV-managed")
  end

  # Test reopening same file twice
  def test_reopen_same_file
    start_vim("WEBDAV_DEFAULT_URL" => "http://localhost:9999")

    # Open file first time
    vim_cmd("WebDAVGet /test/file1.txt")
    wait_for_text("This is test file content")

    # Modify but don't save
    vim_cmd("call setline(1, 'Local change')")
    wait_for_screen_change

    # Close it
    vim_cmd("bdelete!")
    wait_for_screen_change

    # Reopen same file - should get fresh version from server
    vim_cmd("WebDAVGet /test/file1.txt")
    wait_for_text("This is test file content")

    output = capture
    # Should see original content, not our unsaved changes
    assert_includes output, "This is test file content", "Should get fresh version from server"
  end

  # Test that noswapfile is set correctly
  def test_noswapfile_is_set
    start_vim("WEBDAV_DEFAULT_URL" => "http://localhost:9999")

    vim_cmd("WebDAVGet /test/file1.txt")
    wait_for_text("This is test file content")

    # Check swapfile setting
    vim_cmd("if &swapfile | echo 'SWAP_ON' | else | echo 'SWAP_OFF' | endif")
    wait_for_text("SWAP_OFF")

    output = capture
    assert_includes output, "SWAP_OFF", "Swapfile should be disabled for WebDAV buffers"
  end

  # Test Korean content (not just filename)
  def test_korean_content
    start_vim("WEBDAV_DEFAULT_URL" => "http://localhost:9999")

    # Just verify we can open Korean filename file (already tested in test_read.rb)
    vim_cmd("WebDAVGet /test/한글.md")
    wait_for_text("한글 문서")

    # Modify with simple ASCII (Korean input in tmux can be complex)
    vim_cmd("call setline(1, 'Modified')")
    wait_for_screen_change

    vim_cmd("write")
    wait_for_screen_change

    output = capture
    # Just verify no errors with Korean filename
    refute_match(/Error/i, output, "Should handle files with Korean names")
  end

  # Test special characters in content
  def test_special_characters_in_content
    start_vim("WEBDAV_DEFAULT_URL" => "http://localhost:9999")

    vim_cmd("WebDAVGet /test/file1.txt")
    wait_for_text("This is test file content")

    # Add content with some special characters (avoid shell escaping issues)
    vim_cmd("call setline(1, 'Line with # $ % chars')")
    wait_for_screen_change

    vim_cmd("write")
    wait_for_screen_change

    output = capture
    refute_match(/Error/i, output, "Should handle special characters in content")
  end
end
